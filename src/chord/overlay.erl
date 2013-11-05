-module(overlay).
-define(Timeout, 500).
-define(Stabilize, 500).
-define(TimeScale, 0.3).
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
      Peer ! {status, Predecessor},
      node(Id, Predecessor, Successor, Store);

    % Vår Successor informerar oss om vem dess Predecessor är
    {status, SuccPred} ->
      Succ = stabilize(SuccPred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    % Run stabilization
    stabilize ->
      {_, Spid}  = Successor,
      Spid ! {request, self()},
      node(Id, Predecessor, Successor, Store);

    % Sätt en Key till Value, på rätt nod. Kommer routas vidare om den inte är till oss.
    {set_key, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    % Hämta värdet för Key, på rätt nod. Kommer routas vidare om den inte är till oss
    {get_key, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    % Ta en del av en annan nods Store, delad på vår Key
    % Om vi får nycklar som inte ska till oss, passa dem vidare bakåt i ringen
    % Om vår predecessor är nil, skicka meddelandet till oss själva med 100 ms delay, så att vi kan ta hand om det senare
    {handover, Elements} ->
      Merged = case Predecessor of
        {Pkey, Ppid} ->
          % Om vi har en predecessor, merge:a in de element som har med oss att göra in i vår Store
          storage:merge(case storage:split(Pkey, Id, Elements) of
              {[], Elements2} ->
                Elements2;
              % Om det kommer element som inte vi har ansvar för, passa dom vidare bakåt i ringen
              {NotOurElements, Elements2} ->
                spawn(fun() -> timer:sleep(round(100 * ?TimeScale)), Ppid ! {handover, NotOurElements} end),
                Elements2
            end, Store);
        nil when Elements == [] ->
          Store;
        % Om vi inte har någon predecessor, skjut upp det hela 100 ms, så tar vi hand om det senare
        nil ->
          Self = self(),
          spawn(fun() -> timer:sleep(round(100 * ?TimeScale)), Self ! {handover, Elements} end),
          Store
      end,
      node(Id, Predecessor, Successor, Merged);

    % Skicka iväg en probe, som tar ett varv i ringen och samlar information
    probe ->
      create_probe(Id, Successor, Predecessor, Store),
      node(Id, Predecessor, Successor, Store);

    % Ta emot en probe som gått hela varvet och printa ut resultaten
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    % Ta emot en probe, och skicka vidare den med vår info inkluderad
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Predecessor, Store),
      node(Id, Predecessor, Successor, Store);

    % Debugga ut att ett inkommet meddelande inte matchar något vi förväntar oss
    Unexpected ->
      io:format("Unexpected message: ~p~n", [Unexpected])
  end.

% Dela vår Store vid Nkey, och skicka ena halvan till Npid. Behåll resten själv
handover(Store, Nkey, Npid, Id) ->
  {Give, Keep} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Give},
  Keep.

% Lägg sätt Key till Value. Om Key inte finns på vår nod, routa meddleandet vidare
add(Key, Value, Qref, Client, Id, Predecessor, {_, Spid}, Store) ->
  case Predecessor of
    % Om vi inte har någon Predecessor är vi antingen den första noden i ringen, eller så
    % håller vi på å stabiliserar oss.
    % Lägg till alla nycklar lokalt och ge bort dem när vi stabiliserar oss om de inte tillhör oss.
    nil ->
      Store2 = storage:add(Key, Value, Store),
      Client ! {Qref, ok, Id},
      Store2;

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
create_probe(Id, {Skey, Spid}, {Pkey, _}, Store) ->
  Spid ! {probe, Id, [{Id, Skey, Pkey, Store}], erlang:now()}.

% Skicka vidare en probe, med vårt Key tillagd till Nodes
forward_probe(Ref, T, Nodes, Id, {Skey, Spid}, {Pkey, _}, Store) ->
  Spid ! {probe, Ref, [{Id, Skey, Pkey, Store}|Nodes], T}.

% Printa ut innehållet i proben och skicka inte vidare.
remove_probe(T, Nodes) ->
  io:format("Received probe:~n"),
  print_node_entry(Nodes),
  io:format("~n").

print_node_entry([]) ->
  ok;
print_node_entry([{Id, Successor, Predecessor, Store}|Entries]) ->
  io:format("Probed ~p: succ: ~p, pred: ~p, store: ~p~n", [Id, Successor, Predecessor, Store]),
  print_node_entry(Entries).


% This is where the magic happens
% Stabilisera vår plats i ringen genom att korrigera vem vår Successor är, baserat på vem vår nuvarande Successors
% Predecessor är.
% Om vår Successors Predecessor är oss själva har vi stabiliserat oss i ringen och allt är soft.
% Om vår Successors Predecessor är emellan oss och vår Successor är vi på fel plats i ringen och måste stega bakåt.
% Om vår Successors Predecessor inte är oss själva, men vi är emellan Successorn och dess Predecessor måste vi berätta
% att vi är Successorns nya Predecessor.
stabilize(SuccPred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case SuccPred of
    % Vår Successor har ingen predecessor, vi är antaglingen den enda noden i ringen och pratar med oss själva,
    % eller så är vi nr 2 in i ringen.
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;

    % Vi har stabiliserat oss, vi är vår Successors Predecessor, allt är frid och fröjd. Gör inget
    {Id, _} ->
      Successor;

    % Vår Successor har sig själv som predecessor, vi är förmodligen nr 2 in i ringen.
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

    % Vår successor har redan en annan Predecessor
    % Om Predecessorns Key ligger mellan vår och vår Successors är vi på
    % fel plats i ringen och måste stega bakåt genom att sätta Successorns Predecessor som vår Successor.
    % Om den inte är det ska vi hoppa in i mitten, mellan vår Successor och dess Predecessor
    {Pkey, _} ->
      case key:between(Pkey, Id, Skey) of
        % Vår Successors Predecessor är emellan oss och vår Successor. Stega bakåt i ringen tills vi hittar vår plats
        true ->
          SuccPred;

        % Vår Successors Predecessor är bakom oss i ringen, säg till vår Successor att vi ska hoppa in imellan
        % som dess nya Predecessor
        false ->
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
      Keep = handover(Store, Nkey, Npid, Id),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      % Om Nkey är mellan vår aktuella Predecessor och oss, då ska den bli vår nya predecessor.
      % Ge bort en bit av vår store och sätt Predecessor
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Store, Nkey, Npid, Id),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.

% Schemalägg att meddelandet "stabilize" ska skickas till oss själva med intervallet Stabilize
% (intervallet definieras högst upp i filen)
schedule_stabilize() ->
  timer:send_interval(round(?Stabilize * ?TimeScale), self(), stabilize).

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

% Testa om saker verkar funka. Skapa 5 noder och lägg till 4 nycklar på olika noder.
% Försök slå upp de fyra nycklarna, vänta lite, och försök slå upp dem igen
test() ->
  random:seed(erlang:now()),                             % Se till så att randomgeneratorn ger unik random för varje körning,
  Key1 = key:generate(),
  Key2 = key:generate(),
  Key3 = key:generate(),
  Key4 = key:generate(),
  Node1 = start(key:generate()),
  spawn(fun() -> send_probe(Node1) end),
  Node2 = start(key:generate(), Node1),
  set_key(Node2, Key1, msg1),
  Node3 = start(key:generate(), Node2),
  timer:sleep(round(1000 * ?TimeScale)),
  Node4 = start(key:generate(), Node1),
  set_key(Node3, Key2, msg2),
  Node5 = start(key:generate(), Node3),
  set_key(Node1, Key3, msg3),
  timer:sleep(round(1000 * ?TimeScale)),
  set_key(Node5, Key4, msg4),
  timer:sleep(round(2500 * ?TimeScale)),
  get_key(Node5, Key1),
  get_key(Node3, Key2),
  get_key(Node4, Key3),
  get_key(Node5, Key4),
  timer:sleep(round(10000 * ?TimeScale)),
  get_key(Node5, Key1),
  get_key(Node3, Key2),
  get_key(Node4, Key3),
  get_key(Node5, Key4),
  timer:sleep(round(2000 * ?TimeScale)).