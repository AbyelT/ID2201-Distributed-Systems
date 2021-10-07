-module(vector).

-export([zero/1, inc/2, merge/2, leq/2]).
-export([clock/1, update/3, safe/2]).

%% return initial Vector value e.g. <{joe, 0}, {rob, 0}>
zero(Nodes) -> 
    C = fun(Name) -> {Name, 0} end,
    lists:map(C, Nodes).

%% return incremented Vector V
inc(Name, V) ->
    %io:format("name: ~w~nvector: ~p~n", [Name, V]),
    {_, T} = lists:keyfind(Name, 1, V),
    {Name, lists:keyreplace(Name, 1, V, {Name, T+1})}.

%% return the maximum value of two Vector clocks
merge([], []) ->
    [];
merge([Vi|VectorA], [Vj|VectorB]) ->
    [lists:max([Vi, Vj])|merge(VectorA, VectorB)].

%% true if Vi is less or equal Vj
leq([], []) ->
    true;
leq([A|Vi], [B|Vj]) ->
    case A =< B of 
        true -> leq(Vi, Vj);
        false -> false
    end.

%% returns a clock that keeps track of
%% each node and their vector
clock(Nodes) ->
    C = fun(Name) -> {Name, vector:zero(Nodes)} end,
    lists:map(C, Nodes).

%% update the clock with given Node and Vector
update(Node, Vector, Clock) ->
    {_, OldVector} = lists:keyfind(Node, 1, Clock),
    case leq(OldVector, Vector) of
        true ->
            NewVector = merge(OldVector, Vector),
            IncVector = inc(Node, NewVector),
            lists:keyreplace(Node, 1, Clock, IncVector);
        false ->
            IncVector = inc(Node, OldVector),
            lists:keyreplace(Node, 1, Clock, IncVector)
    end.

%% checks if it is safe to log events 
%% at a given Vector time
safe(_, []) ->
    true;
safe(Vector, [{_, OldVector}|Rest]) -> 
    case leq(Vector, OldVector) of
        true -> safe(Vector, Rest);
        false -> false
    end.
    