%% Copyright
-module(practice).
-author("lingon").

%% API
-export([prac/0]).

prac() ->
  io:format("~p~n", [http:parse_request(http:get("/hello/asd.txt?key=value&key2=value2"))]),
  io:format("~p~n", [http:parse_request(http:get("/hello/asd.txt"))]),
  io:format("~p~n", [http:parse_request(http:get("/hello/asd.txt?key"))]),
  io:format("~p~n", [http:parse_request(http:get("/hello/asd.txt?key="))]).
  % io:format("result: ~p~n", [isendofrequest(<<"sadsad">>)]),
  % io:format("result: ~p~n", [isendofrequest(<<"sadsad\r\n\r\n">>)]),
  % io:format("result: ~p~n", [merge([4,6,3,5,2,1])]).

isendofrequest(Bin) ->
  isendofrequest_(binary:list_to_bin(lists:reverse(binary:bin_to_list(Bin)))).

isendofrequest_(<<"\n\r\n\r",T/binary>>) ->
  true;
isendofrequest_(L) ->
  false.

count([]) ->
  0;
count([_|T]) ->
  1 + count(T).

sum([]) ->
  0;
sum([H|T]) ->
  H + sum(T).

append([], L) ->
  L;
append([H|T], L) ->
  [H|append(T, L)].

collect(L, P) ->
  collect(L, P, []).

collect([H|T], 0, R) ->
  R;
collect([H|T], P, R) ->
  collect(T, P - 1, R ++ [H]).

collectb(L, 0) ->
  L;
collectb([H|T], P) ->
  collectb(T, P - 1).

merge([I]) ->
  [I];
merge(L) ->
  LeftCollection = collect(L, length(L) div 2),
  RightCollection = collectb(L, length(L) div 2),
  Left = merge(LeftCollection),
  Right = merge(RightCollection),
  merge(Left, Right, []).

merge([], [], Res) ->
  lists:reverse(Res);
merge([], [R], Res) ->
  merge([], [], [R|Res]);
merge([L], [], Res) ->
  merge([], [], [L|Res]);
merge([LH|LT], [], Res) ->
  merge([], [], [LH|LT] ++ Res);
merge([], [RH|RT], Res) ->
  merge([], [], [RH|RT] ++ Res);
merge([LH|LT], [RH|RT], Res) ->
  if LH =< RH ->
    merge(LT, [RH|RT], [LH|Res]);
  true ->
    merge([LH|LT], RT, [RH|Res])
  end.