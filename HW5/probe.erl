-module(probe).

-export([create_probe/2, remove_probe/2, forward_probe/5]).

create_probe(Id, {_, Spid}) ->
    %io:format("successor: ~w~n", [Spid]),        
    T = erlang:system_time(micro_seconds),
    Spid ! {probe, Id, [self()], T}.

remove_probe(T, Nodes) ->
    Tend = erlang:system_time(micro_seconds),
    Time = Tend - T,
    % io:format("~w - start time: ~w microsec~n", [self(), T]),
    % io:format("~w - end time: ~w microsec~n", [self(), Tend]),
    io:format("~w - time: ~w microsec, nodes:~w~n", [self(), Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
    %io:format("successor: ~w~n", [Spid]),        
    Spid ! {probe, Ref, [self()|Nodes], T}.

