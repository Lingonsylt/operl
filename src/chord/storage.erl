-module(storage).

%% API
-export([create/0, add/3, lookup/2, merge/2, split/3, test/0]).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value}|Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

% Dela en Store i två delar, en med alla nycklar som inte är mellan SplitFrom och SplitTo, och en som är alla
% nycklar som är emellan.
split(SplitFrom, SplitTo, Store) ->
  split(SplitFrom, SplitTo, Store, [], []).

split(_, _, [], NotBetween, Between) ->
  {NotBetween, Between};

split(SplitFrom, SplitTo, [{Key, Value}|Store], NotBetween, Between) ->
  case key:between(Key, SplitFrom, SplitTo) of
    false ->
      split(SplitFrom, SplitTo, Store, [{Key, Value}|NotBetween], Between);
    true ->
      split(SplitFrom, SplitTo, Store, NotBetween, [{Key, Value}|Between])
  end.

merge(Store1, Store2) ->
  Store1 ++ Store2.

test() ->
  Store = create(),
  Store2 = add(32, 0, Store),
  Store3 = add(2, 0, Store2),
  Store4 = add(19, 0, Store3),
  Store5 = add(8, 0, Store4),
  Store6 = add(7, 0, Store5),
  Store7 = add(1, 0, Store6),
  io:format("~p~n", [split(0, 3, Store7)]),
  io:format("~p~n", [split(5, 2, Store7)]),
  io:format("~p~n", [split(8, 6, Store7)]),
  io:format("~p~n", [split(35, 69, Store7)]).