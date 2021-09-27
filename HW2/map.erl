-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

%% empty list
new() ->
    [].

%% update the map with given Node and its close gateways
update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node, 1, Map), 
    [{Node, Links}|NewMap].
    

%% Find the other gateways that can reach Node
reachable(Node, Map) -> 
    case lists:keyfind(Node, 1, Map) of
        {_, Lists} -> Lists;
        false -> []
end.

%% return all nodes from the map
all_nodes([]) ->
    [];
all_nodes(Map) ->
    NewMap = lists:map(fun({X,Y}) -> [X|Y] end, Map),
    lists:flatten(NewMap).
