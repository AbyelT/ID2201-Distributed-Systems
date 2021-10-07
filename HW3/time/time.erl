-module(time).

-export([zero/0, inc/2, merge/2, leq/2]).
-export([clock/1, update/3, safe/2]).


%% return initial Lamport value (0)
zero() -> 
    0.

%% return incremented time T
inc(Name, T) ->
    {Name, T+1}.

%% return the maximum value of two Lamport clocks
merge(Ti, Tj) ->
    lists:max([Ti, Tj]).

%% true if Ti is less or equal Tj
leq(Ti, Tj) ->
    Ti =< Tj.

%% returns a clock that keeps track of
%% each node and their recent time stamp
clock(Nodes) ->
    C = fun(Name) -> {Name, 0} end,
    lists:map(C, Nodes).

%% update the clock with given Node and Time
update(Node, Time, Clock) ->
    {_, OldTime} = lists:keyfind(Node, 1, Clock),
    case leq(OldTime, Time) of
        true ->
            NewTime = merge(OldTime, Time),
            IncStamp = inc(Node, NewTime),
            lists:keyreplace(Node, 1, Clock, IncStamp);
        false ->
            IncStamp = inc(Node, OldTime),
            lists:keyreplace(Node, 1, Clock, IncStamp)
    end.

%% checks if it is safe to log events 
%% at a given Time
safe(_, []) ->
    true;
safe(Time, [{_, OldTime}|Rest]) -> 
    case leq(Time, OldTime) of
        true -> safe(Time, Rest);
        false -> false
    end.
    