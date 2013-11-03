-module(http).
-export([parse_request/1, ok/1, not_found/1, internal_server_error/0, get/1]).

parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

parse_url([$?|R0]) ->
  {[], R0};
parse_url([]) ->
  {[], []};
parse_url([C|R0]) ->
  {Rest, R1} = parse_url(R0),
  {[C|Rest], R1}.

parse_key([$=|R0]) ->
  {[], R0};
parse_key([]) ->
  {[], []};
parse_key([C|R0]) ->
  {Rest, R1} = parse_key(R0),
  {[C|Rest], R1}.

parse_value([]) ->
  {[], []};
parse_value([$&|R0]) ->
  {[], R0};
parse_value([C|R0]) ->
  {Rest, R1} = parse_value(R0),
  {[C|Rest], R1}.

parse_args([]) ->
  [];
parse_args(R0) ->
  {Key, R1} = parse_key(R0),
  {Value, R2} = parse_value(R1),
  Args = parse_args(R2),
  [{Key, Value}|Args].

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

not_found(Body) ->
  "HTTP/1.1 404 Not Found\r\n" ++ "\r\n" ++ Body.

internal_server_error() ->
  "HTTP/1.1 500 Internal Server Error\r\n".


get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

request_line([$G, $E, $T, 32 |R0]) ->
  {URI, R1} = request_uri(R0),
  {URL, URLRest} = parse_url(URI),
  Args = parse_args(URLRest),
  {Ver, R2} = http_version(R1),
  [13,10|R3] = R2,
  {{get, {URL, Args}, Ver}, R3}.

request_uri([32|R0])->
  {[], R0};
request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0),
  {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

headers([13,10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.

header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}.

message_body(R) ->
  {R, []}.