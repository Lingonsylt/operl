-module(server).
-export([start/1,run/1,ver/0,server_start/1, server_handle/1, test/0]).

% Parse:a första command-line-argumentet som en int och passa till start/0
run([PortString|_]) ->
  start(list_to_integer(PortString)).

% Starta servern på en viss port
start(Port) ->
  % Printa till stout
  io:format("Spawning server on port: ~p ...~n", [Port]),

  ChordPid = overlay:start(key:generate()),
  register(chordpeer, ChordPid),

  % Starta servern
  spawn(fun() -> server_start(Port) end).

test() ->
  server:start(8080),
  overlay:set_key(chordpeer, key:hexhash("hello.txt"), "Hello Mr. World!").

ver() ->
  "1".
% Bind till en socket på Port och börja lyssna, och starta socket-accept
server_start(Port) ->
  {ok, ServerSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  io:format("Listening for connections on ~p...~n", [Port]),
  % Börja acceptera requests med server_accept som rekurserar för evigt
  server_accept(ServerSock).

% Acceptera en connection och starta en ny process för att hantera den
server_accept(ServerSock) ->
  {ok, Sock} = gen_tcp:accept(ServerSock),
  % Starta en ny erlangprocess, som kör funktionen server_handle, i paketet server, med argumenten Sock
  spawn(server, server_handle, [Sock]),

  % Rekursera för evigt
  server_accept(ServerSock).

% Läs data på socketen tills vi får en "\r\n\r\n"-sekvens, eller andra sidan stänger socketen
% Parse:a requestet och skapa en response, skicka tillbaka på socketen och stäng
% När funktionen returnerar stängs processen av
server_handle(Sock) ->
  {ok, Bin} = do_recv(Sock, []),
  Response = server_parse(Bin),
  gen_tcp:send(Sock, Response),
  ok = gen_tcp:close(Sock).

% Parse:a requesten till ett request-objekt, och returnera ett response baserat på det
server_parse(Data) ->
  RequestData = binary_to_list(Data), % Konvertera binärdata till text
  Request = http:parse_request(RequestData),
  reply(Request).

% Svara med ett HTTP 200 med innehållet i filen om filen finns. Annars returnera 404 (eller 500 om det går åt pepparn)
reply({{get, {URL, Args}, _}, _, _}) ->
  [_|LocalURL] = URL, % Plocka bort första tecknet (en slash) från urlen

  Qref = make_ref(), % Skapa ett unikt ID på vår request, så att vi kan skriva ut rätt svar när det kommer
  chordpeer ! {get_key, key:hexhash(LocalURL), Qref, self()},
  receive % Vänta på svaret och skriv ut det när det kommer
    {Qref, Result, Nkey} ->
      io:format("Lookup for ~p on ~p: ~p~n", [LocalURL, Nkey, Result]),
      case Result of
        false ->
          http:not_found("File nout found: " ++ LocalURL);
        {_, Res} ->
          http:ok(Res)
      end
  after 3000 ->
    http:ok("Sorry timeout!")
  end.

% Läs data från socketen tills vi får en "\r\n\r\n"-sekvens, eller andra sidan stänger kopplingen
do_recv(Sock, ReceivedData) ->
  % Plocka fram de sista fyra bytesen ur datan, om vi fått fyra bytes än
  if byte_size(ReceivedData) >= 4 ->
    LastFourBytes = binary:part(ReceivedData, byte_size(ReceivedData), -4);
  true ->
    LastFourBytes = <<"">>
  end,

  % Returnera datan om vi får slutsekvensen
  if LastFourBytes =:= <<"\r\n\r\n">> ->
    {ok, ReceivedData};
  true ->
    case gen_tcp:recv(Sock, 0) of
      {ok, NewReceivedData} ->
        % Rekursera om vi får data
        do_recv(Sock, list_to_binary([ReceivedData, NewReceivedData]));
      {error, closed} ->
        % Returnera och logga fel om andra sidan stänger kopplingen
        io:format("Request error: connection closed!"),
        {ok, list_to_binary(ReceivedData)}
    end
  end.