-module(probe).

-export([create_probe/2, remove_probe/2, forward_probe/5]).

create_probe(Id, {_, Spid}) ->
    io:format("successor: ~w~n", [Spid]),        
    T = erlang:system_time(micro_seconds),
    Spid ! {probe, Id, [Id], T}.

remove_probe(T, Nodes) ->
    Tend = erlang:system_time(micro_seconds),
    Time = Tend - T,
    io:format("~w - time: ~w µs, nodes:~w~n", [self(), Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
    io:format("successor: ~w~n", [Spid]),        
    Spid ! {probe, Ref, [Id|Nodes], T}.

