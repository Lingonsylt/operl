-module(overlay).
-define(Timeout, 500).
-define(Stabilize, 500).

-export([start/1, start/2, test/0, set_key/3, get_key/2]).

% Starta den första noden i ringen
start(Id) ->
  % Stata en nod och sätt Successor till nil (nil är ej ett reserverat ord, utan en vanlig atom)
  start(Id, nil).

% Lägg till en nod i ringen genom att ansluta till Peer
start(Id, Peer) ->
  spawn(fun() -> init(Id, Peer) end).                    % Starta noden i en ny process

% Starta en ny nod, och joina hos Peer
init(Id, Peer) ->
  Predecessor = nil,                                     % Vi har ingen bakom oss när vi startar
  {ok, Successor} = connect(Id, Peer),                   % Fråga successorns key och sätt Successor = {Skey, Peer}
  schedule_stabilize(),                                  % Schemalägg stabilisering. Detta kommer att forma ringen
  node(Id, Predecessor, Successor, storage:create()).    % Starta noden genom att rekursera node() för evigt

% Om Peer är nil, sätt Successor till oss själva, en ring med 1 nod
connect(Id, nil) ->
  {ok, {Id, self()}};

% Skicka en fråga till Peer och be om dess key. Returnera {ok, {Key, Peer}}
connect(_Id, Peer) ->
  Qref = make_ref(),                                     % Skapa en unik identifierare för vår request
  Peer ! {key, Qref, self()},                            % Be Peer att skicka oss dess key
  receive                                                % Ta emot en nyckel i det meddelande som matchar Qref
    {Qref, Skey} ->
      {ok, {Skey, Peer}}                                 % Returnera
  after ?Timeout ->
    io:format("Time out: no response~n",[]),
    error                                                     % Returnera atomen error. Kommer resultera i fel i "{ok, Successor} ="
                                                              % i init/2.
  end.

% Starta en oändlig rekursering, vår main-loop
node(Id, Predecessor, Successor, Store) ->
  receive
    % En nod vill veta vår key, svara med {Qref, Id}
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    % En ny nod berättar att den vill bli vår Predecessor
    % Acceptera den ifall den är valid
    {notify, New} ->
      {Pred, Store2} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Store2);

    % Vår Predecessor vill veta vilken vår Predecessor är
    % Svara med Predecessor == nil ? nil : Predecessor.
    {request, Peer} ->
      % io:format("~p: got request for predecessor: ~p~n", [Id, Predecessor]),
      request(Peer, Predecessor),                             % request/2 är helt onödig och ska tast bort!
      node(Id, Predecessor, Successor, Store);

    % Our successor informs us about its predecessor
    % Vår Successor informerar oss om vem dess Predecessor är
    {status, Pred} ->
      % io:format("~p: response with current predecessor: ~p~n", [Id, Pred]),
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    % Run stabilization
    stabilize ->
      Succ = stabilize(Successor), % of
      node(Id, Predecessor, Succ, Store);

    % Sätt en Key till Value, på rätt nod. Kommer routas vidare om den inte är till oss.
    {set_key, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    % Hämta värdet för Key, på rätt nod. Kommer routas vidare om den inte är till oss
    {get_key, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    % Ta en del av en annan nods Store, delad på vår Key
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);

    % Skicka iväg en probe, som tar ett varv i ringen och samlar information
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);

    % Ta emot en probe som gått hela varvet och printa ut resultaten
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    % Ta emot en probe, och skicka vidare den med vår info inkluderad
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);

    % Debugga ut att ett inkommet meddelande inte matchar något vi förväntar oss
    Unexpected ->
      io:format("Unexpected message: ~p~n", [Unexpected])
  end.

% Dela vår Store vid Nkey, och skicka ena halvan till Npid. Behåll resten själv
handover(Store, Nkey, Npid, Id) ->
  {Give, Keep} = storage:split(Nkey, Store),
  Npid ! {handover, Give},
  Keep.

% Lägg sätt Key till Value. Om Key inte finns på vår nod, routa meddleandet vidare
add(Key, Value, Qref, Client, Id, Predecessor, {_, Spid}, Store) ->
  case Predecessor of
    % TODO: Skriv comment
    nil ->
      Spid ! {set_key, Key, Value, Qref, Client},
      Store;

    % Om Key är mellan vår Key och vår Predecessors Key är det vårt ansvar. Lägg till den lokalt
    % Om den inte är det, routa vidare till vår Successor.
    {Pkey, _} ->
      case key:between(Key, Pkey, Id) of
        true ->
          Store2 = storage:add(Key, Value, Store),
          Client ! {Qref, ok, Id},
          Store2;
        false ->
          Spid ! {set_key, Key, Value, Qref, Client},
          Store
      end
  end.


% Hämta värdet på Key
lookup(Key, Qref, Client, Id, Predecessor, Successor, Store) ->
  case Predecessor of
    % Om vi inte har någon Predecessor är vi den första noden i ringen - lägg till lokalt
    nil ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result, Id};
    {Pkey, _} ->

      % Om Key är mellan vår Key och vår Predecessors Key är det vårt ansvar. Hämta den från
      % den lokala Store:en
      % Om den inte är det, routa vidare till vår Successor.

      case key:between(Key, Pkey, Id) of
        true ->
          Result = storage:lookup(Key, Store),
          Client ! {Qref, Result, Id};
        false ->
          {_, Spid} = Successor,
          Spid ! {get_key, Key, Qref, Client}
      end
    end.

% Skapa en "probe", ett meddelande som tar ett varv i ringen och samlar data om vår overlay
% Tagga den med vår Key så att vi vet när den är tillbaka ifall flera probes är på gång
create_probe(Id, {Skey, Spid}) ->
  io:format("Sending probe to from ~p to ~p~n", [Id, Skey]),
  Spid ! {probe, Id, [Id], erlang:now()}.

% Skicka vidare en probe, med vårt Key tillagd till Nodes
forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
  timer:sleep(500),
  % io:format("~p: Forwarding probe originating from ~p to ~p~n", [Id, Ref, Skey]),
  Spid ! {probe, Ref, [Id|Nodes], T}.

% Printa ut innehållet i proben och skicka inte vidare.
remove_probe(T, Nodes) ->
  io:format("Received probe after ~p seconds, going through ~p~n", [T, Nodes]).

% Helt onödig?
request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

stabilize({Skey, Spid}) ->
  Spid ! {request, self()},
  {Skey, Spid}.

% Dude, where's my car?
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    % We're the second node entering the ring
    % Why can this happen?
    nil ->
      % io:format("~p: notifying ~p that we're its new predecessor (nil)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

    % We're stabilized, our successors predecessor is us
    {Id, _} ->
      Successor;

    % We're the second node entering the ring
    {Skey, _} ->
      % io:format("~p: notifying ~p that we're its new predecessor (second)~n", [Id, Skey]),
      Spid ! {notify, {Id, self()}},
      Successor;

    % There is already another predecessor
    {Pkey, Ppid} ->
      case key:between(Pkey, Id, Skey) of
      % The predecessor of our successor is in between us and our successor. Try stepping backwards in the ring
        true ->
          % io:format("~p: stepping backards to new successor ~p~n", [Id, Pred]),
          Pred;

      % The predecessor of our successor is behind us, notify our successor that we're its new predecessor
        false ->
          % io:format("~p: notifying ~p that we're its new predecessor (~p in between ~p and ~p)~n", [Id, Skey, Id, Pkey, Skey]),
          Spid ! {notify, {Id, self()}},
          Successor

      end
  end.

% En annan nod har berättat att den är vår nya Predecessor. Om den verkligen är det, sätt den till vår
% Predecessor och ge den den del av vår Store som nu är dess ansvar, baserat på bådas Key
notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    % Om vi inte har någon Predecessor är vi den första noden i ringen, ge bort en bit av
    % vår Store och sätt Predecessor till den nya: {Nkey, Npid}
    nil ->
      % io:format("~p: updating predecessor ~p (nil)~n", [Id, {Nkey, Npid}]),
      Keep = handover(Store, Nkey, Npid, Id),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      % Om Nkey är mellan vår aktuella Predecessor och oss, då ska den bli vår nya predecessor.
      % Ge bort en bit av vår store och sätt Predecessor
      case key:between(Nkey, Pkey, Id) of
        true ->
          % io:format("~p: updating predecessor ~p (~p in between ~p and ~p)~n", [Id, {Nkey, Npid}, Nkey, Pkey, Id]),
          Keep = handover(Store, Nkey, Npid, Id),
          {{Nkey, Npid}, Keep};
        false ->
          % io:format("~p: not updating predecessor ~p~n", [Id, {Nkey, Npid}]),
          {Predecessor, Store}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

% Be Pid skicka en probe varannan sekund
send_probe(Pid) ->
  timer:sleep(2000),
  Pid ! probe, % Be Pid skicka en probe
  send_probe(Pid).

% Be Node att sätta Key till Value, och skriv ut resultatet
set_key(Node, Key, Value) ->
  spawn(fun() -> % Kör en lambdafunktion i en ny process, så att vi kan returnera direkt från set_key
    Qref = make_ref(), % Skapa ett unikt ID på vår request, så att vi kan skriva ut rätt svar när det kommer
    Node ! {set_key, Key, Value, Qref, self()}, % Be Node att sätta Key till Value, och skicka svaret till oss (till self())
    receive % Vänta på svaret och skriv ut det när det kommer
      {Qref, ok, NKey} ->
        io:format("Added (~p,~p) to ~p~n", [Key, Value, NKey])
    end
  end).

% Hämta värdet på en nyckel
get_key(Node, Key) ->
  spawn(fun() -> % Kör en lambdafunktion i en ny process, så att vi kan returnera direkt från get_key
    Qref = make_ref(), % Skapa ett unikt ID på vår request, så att vi kan skriva ut rätt svar när det kommer
    Node ! {get_key, Key, Qref, self()},
    receive % Vänta på svaret och skriv ut det när det kommer
      {Qref, Result, Nkey} ->
        io:format("Lookup for ~p on ~p: ~p~n", [Key, Nkey, Result])
    end
  end).

test() ->
  random:seed(erlang:now()),                             % Se till så att randomgeneratorn ger unik random för varje körning
  TimeScale = 1,
  Key1 = key:generate(),
  Key2 = key:generate(),
  Key3 = key:generate(),
  Key4 = key:generate(),
  Node1 = start(key:generate()),
  Node2 = start(key:generate(), Node1),
  set_key(Node2, Key1, msg1),
  Node3 = start(key:generate(), Node2),
  spawn(fun() -> send_probe(Node1) end),
  timer:sleep(1000 * TimeScale),
  Node4 = start(key:generate(), Node1),
  set_key(Node3, Key2, msg2),
  Node5 = start(key:generate(), Node3),
  set_key(Node1, Key3, msg3),
  timer:sleep(1000 * TimeScale),
  set_key(Node5, Key4, msg4),
  timer:sleep(2500 * TimeScale),
  get_key(Node5, Key1),
  get_key(Node3, Key2),
  get_key(Node4, Key3),
  get_key(Node5, Key4),
  timer:sleep(10000 * TimeScale),
  get_key(Node5, Key1),
  get_key(Node3, Key2),
  get_key(Node4, Key3),
  get_key(Node5, Key4),
  timer:sleep(2000 * TimeScale).