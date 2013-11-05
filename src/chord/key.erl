-module(key).
-export([generate/0,between/3, hexhash/1]).

generate() ->
  Hash = hexhash(),
  lists:nthtail(length(Hash) - 4, Hash).

% Svara på om en nyckel är mellan två andra nycklar i en ring, exklusive From, inklusive To.
% Eftersom det är en ring så är alla nycklar mellan X och X, och alla utom 1 mellan 1 och 0.
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

