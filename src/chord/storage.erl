-module(storage).

%% API
-export([create/0, add/3, lookup/2, merge/2, split/2, test/0]).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value}|Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(Key, Store) ->
  split(Key, Store, [], []).

split(_, [], Lower, HigherOrEqual) ->
  {Lower, HigherOrEqual};

split(SplitKey, [{Key, Value}|Store], Lower, HigherOrEqual) ->
  case Key < SplitKey of
    true ->
      split(SplitKey, Store, [{Key, Value}|Lower], HigherOrEqual);
    false ->
      split(SplitKey, Store, Lower, [{Key, Value}|HigherOrEqual])
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
  io:format("~p~n", [split(0, Store7)]),
  io:format("~p~n", [split(5, Store7)]),
  io:format("~p~n", [split(8, Store7)]),
  io:format("~p~n", [split(35, Store7)]).