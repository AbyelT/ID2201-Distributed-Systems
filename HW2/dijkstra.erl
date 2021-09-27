-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

%% returns the length of the shortest path to the
%% node or 0 if the node is not found.
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of 
        {Node, N, _} -> 
            % io:format("found entry~n"),
            N;
        false -> 
            % io:format("no entry~n"),
            0
    end.

%% replaces the entry for Node in Sorted list with 
%% a new entry having length N and Gateway. 
replace(Node, N, Gateway, Sorted) ->
    NewSorted = lists:keydelete(Node, 1, Sorted),
    if 
        NewSorted == Sorted -> 
            % io:format("replace: nothing changed~n"),
            Sorted;
        true -> 
            % io:format("replace: insert new element~n"),
            insert({Node, N, Gateway}, NewSorted)
    end.


%% Sorts the given list in ascending order of the N
insert(A, []) -> 
    % io:format("Insert is done: ~p~n", [A]),
    [A];
insert({_, N, _}=A, [{_, N2, _}=B|Rest]=List) ->
    if 
        N > N2 ->
            % io:format("~b is bigger than ~w~n", [N, N2]),
            [B|insert(A, Rest)];
        true ->
            % io:format("~b is smaller than ~w~n", [N, N2]),
            [A|List]
    end.

%% update the list Sorted given that Node can be reached in N hops using Gateway.
%% If no entry is found then no new entry is added. 
%% The existing entry is replaced If the given entry has a shorter path
update(Node, N, Gateway, Sorted) -> 
    Len = entry(Node, Sorted),
    if 
        N < Len -> 
            % io:format("~b is less than ~w~n", [N, Len]),
            replace(Node, N, Gateway, Sorted);
        true -> 
            % io:format("~b is bigger|equal than ~b~n", [N, Len]),
            Sorted  
    end.

%%  construct a table given a sorted list
%%  of nodes, a map and a table constructed so far, CHANGED
iterate([], _, Table) ->
    Table;
iterate([{_, 'inf', _}|_], _, Table) ->
    Table;
iterate([{Node, N, Gateway}|Sorted], Map, Table) ->
    Links = map:reachable(Node, Map),
    UpdateSorted = lists:foldl(fun(Link, Sorted1) -> update(Link, N+1, Gateway, Sorted1) end, Sorted, Links),
    ExtendTable = [{Node, Gateway}|Table],                      
    iterate(UpdateSorted, Map, ExtendTable).

%% creates a routing table, CHANGED
table(Gateways, Map) -> 
    Nodes = map:all_nodes(Map),
    Union = lists:usort(Nodes ++ Gateways),
    InitialSorted = initialiseSort(Gateways, Union),
    iterate(InitialSorted, Map, []).

%% initializes a sorted list, CHANGED
initialiseSort(Gateways, Union) ->
    DummySorted = lists:map(fun(X) -> {X, inf, unknown} end, Union),
    RealSorted = lists:foldl(fun(Gateway, Sorted) -> update(Gateway, 0, Gateway, Sorted) end, DummySorted, Gateways),
    io:format("~w~n", [RealSorted]),
    RealSorted.

%% search for the shortest path for the given Node
route(Node, Table) -> 
    case lists:keyfind(Node, 1, Table) of 
        {_, Gateway} -> {ok, Gateway};
        false -> notfound
    end.

%% OLD ITERATE CODE
% iterate([{Node, N, Gateway}|Rest], Map, Table) ->
%     NewSorted = addNewEntries(Node, N, Gateway, Map, Rest),    
%     ExtendTable = [{Node, Gateway}|Table],                     
%     iterate(NewSorted, Map, ExtendTable).

% addNewEntries([], _, _, _, Sorted) ->
%     io:format("done: ~p~n", [Sorted]),
%     Sorted;
% addNewEntries(Node, N, Gateway, Map, Sorted) ->
%     case map:reachable(Node, Map) of
%         [] -> 
%             io:format("no nearby nodes for ~p~n", [Node]),
%             update(Node, N, Gateway, Sorted);
%         [First|Rest] -> 
%             io:format("found nearby nodes for ~s~n", [Node]),
%             AddedElem = addNewEntries(First, N+1, Gateway, Map, Sorted), %%updatera map?
%             addNewEntries(Rest, N, Gateway, Map, AddedElem)
%     end.