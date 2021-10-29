-module(key).
-define(billion, 1000000000).

-export([generate/0, between/3]).

%% generates a numerical key between 1 and million
generate() ->
    random:uniform(?billion).

%% checks if a given key is inside a partly closed interval
%% e.g (From, To]. If from and to are equal anything is between

between(Key, From, To) ->
    if
        From == To ->     % From and To are equal
            true;
        From > To ->     % From is larger than To e.g 7 > 3
            if 
                Key > From -> true;   % the key is in the interval (From, 0)
                Key =< To -> true;    % the key is in the interval [0, To]
                Key > To -> false    % the key is outside both intervals
            end;
        true ->    % To is larger than From
            (Key > From) and (Key =< To)
    end.

% between(Key, From, From) ->
%     true;
% between(Key, From, To) ->
%     case From > To of 
%         true ->     % From is larger than To e.g 3 > 0, assume From and To are adjacent
%             Key > From and Key > ;      
%         false ->    % To is larger than From
%             (Key > From) and (Key =< To)
%     end.
     