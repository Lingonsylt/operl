-module(key).

%% API
-export([generate/0,between/3]).

generate() ->
  random:uniform(1000000000).

between(Key, From, To) ->
  if
    To == From ->
      true;
    To > From ->
      (Key > From) and (Key =< To);
    true ->
      (Key > From) or (Key =< To)
  end.


