-module(key).

%% API
-export([generate/0,between/3, hexhash/1]).

generate() ->
  Hash = hexhash(),
  lists:nthtail(length(Hash) - 4, Hash).

between(Key, From, To) ->
  if
    To == From ->
      true;
    To > From ->
      (Key > From) and (Key =< To);
    true ->
      (Key > From) or (Key =< To)
  end.

hexhash() ->
  hexhash(integer_to_list(random:uniform(1000000000))).
hexhash(Str) ->
  lists:flatmap(fun(X) -> integer_to_list(X, 16) end, binary_to_list(crypto:hash(sha, Str))).

