-module(vectorloggy).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = vector:clock(Nodes),    % for vectors
    loop(Clock, []).

%% added clock and hold-back queue
loop(Clock, Queue) ->
    receive
        {log, From, Time, Msg} ->
            NewClock = vector:update(From, Time, Clock),        %% or vector
            NewQueue = [{From, Time, Msg}|Queue],               %% 2. add message to queue
            {Printable, Remained} = lists:partition(fun({_, T, _}) -> vector:safe(T, NewClock) end, NewQueue),
            lists:foreach(fun({N, T, M}) -> log(N, T, M) end, Printable),
            loop(NewClock, Remained);
        stop ->
            io:format("remaining queue: ~p~n", [Queue]),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
