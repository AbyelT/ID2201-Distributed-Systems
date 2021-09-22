-module(test2).
-export([parse/0, bench/4]).

parse() ->
    http:parse_request("GET /foo HTTP/1.1\r\nUser-Agent: Test\r\nAccept: anything\r\n\r\nThis is the body").

% Modified for task 4.1 - Abyel
bench(Host, Port, Threads, Requests) ->
    Start = erlang:system_time(micro_seconds),
    parallel(Threads, Host, Port, Requests, self()),
    collect(Threads),
    Finish = erlang:system_time(micro_seconds),
    Time = Finish - Start,
    io:format(" ~wx~w requests in ~w microsec~n", [Threads,Requests,Time]).


parallel(0, _, _, _, _) ->
    ok;
parallel(C, Host, Port, N, Ctrl) ->
    spawn(fun() -> report(N, Host, Port, Ctrl) end),
    parallel(C-1, Host, Port, N, Ctrl).


report(N, Host, Port, Ctrl) ->
    run(N, Host, Port),
    Ctrl ! ok.


collect(0) ->
    ok;
collect(N) ->    
    receive 
	ok ->
	    collect(N-1)
    end.

run(0, _, _) ->
    ok;
run(N, Host, Port) ->
    %%io:format("sending request ~w~n", [N]),
    request(Host, Port),
    %%dummy(Host, Port),
    run(N-1, Host, Port).

dummy(_, _) ->
     ok.


request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
     	{ok, _} ->
     	    ok;
     	{error, Error} ->
     	    io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
    
    


