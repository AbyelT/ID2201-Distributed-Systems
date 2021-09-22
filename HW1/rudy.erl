-module(rudy).
-export([init/1]).
-export([start/1, stop/0]).

% STARTS SERVER
start(Port) -> 
    % io:format("START\n"),
    register(rudy, spawn(fun() -> init(Port) end)).

% STOP SERVER
stop() ->
    exit(whereis(rudy), "you are now dead").

% open listening sockter with port
% passes the socket to handler/1
% close the socket
init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen), 
            ok;
        {error, Error} ->
            error
    end.

% listen for incoming connection
% if client connects: pass connection to request/1
% close connection later
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            %io:format("new client!\n"),
            request(Client);
        {error, Error} ->
            error
        end,
    handler(Listen).

% read request from client
% parse with http_parser
% pass request to reply/1
request(Client) -> 
    Recv = gen_tcp:recv(Client, 0),
    case Recv of 
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

% turn reply into HTTP reply
reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).
