-module(http42).
-export([parse_request/1]).
-export([ok/1]).
-export([get/1]).

%% parses incoming requests, 
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    case find_length(R1) of                 
        [] ->  
            {Body, _} = message_body(R2),               %% if Content-Length is not included
            {Request, Headers, Body};                   %%      :read the body as a single string
        Len -> 
            {Body, _} = message_body(R2, Len),       %% else if Content-Length is included
            {Request, Headers, Body}                    %%      :read the body with the given length
        end.

request_line([$G, $E, $T, 32 | R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3}.

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

%% Find the Content-Type header and read the 'length' value
%% Return with empty list when no such header is found 
find_length([$C, $o, $n, $t, $e, $n, $t, $-, $L, $e, $n, $g, $t, $h, $:, 32 | R0])  ->
    {Val, _} = string:to_integer(read_length(R0)),
    %% io:format("value: " ++ [Val] ++ "~n"),
    Val;
find_length([13,10,13,10|_]) ->
    [];
find_length([_|T]) ->
    % %io:format([H] ++ "~n"),
    find_length(T).

%% Extract the value from the Content-Length header
read_length([13,10|_]) ->
    [];
read_length([H|T]) ->
    Val = read_length(T),
    [H|Val].

% normal read of body
message_body(R) ->
    {R, []}.

% reads body until 'len' is zero, also stops reading if
% the body is empty before length
message_body(_, 0) ->
    % io:format("len is ZERO~n"),
    {[], []};
message_body([], _) ->
    % io:format("body was empty before len~n"),
    {[], []};
message_body([C|R], Len) ->
    % io:format("~w~n", [Len]),
    {Rest, R3} = message_body(R, Len - 1),
    {[C|Rest], R3}.

%% returns 200 OK
ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

%% makes a GET request
get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

% message_body([13,10|Rest]) ->
%     io:format("DONE 1~n"),
%     {[], Rest};
% message_body([]) ->
%     io:format("DONE 2~n"),
%     {[], []};
% message_body([13,10|Rest]) ->
%     io:format("one row~n"),
%     {Body, Rest} = message_body([Rest])
%     {[13,10, ], _}
% message_body([C|Rest]) ->
%     io:format("char~n"),
%     {[C|message_body(Rest)], []}.
