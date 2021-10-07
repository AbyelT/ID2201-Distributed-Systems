-module(key).
-define(billion, 1000000000).

-export([generate/0, between/3]).

%% generates a numerical key between 1 and million
generate() ->
    random:uniform(?billion).

%% cheecks if a given key is inside a partly closed interval
%% e.g (From, To]. If from and to are equal anything is between
between(Key, From, From) ->
    true;
between(Key, From, To) ->
    case From > To of 
        true ->     % From is larger than To e.g 3 > 0, assume From and To are adjacent
            (Key > From);       
        false ->    % To is larger than From
            (Key > From) and (Key =< To)
    end.
     