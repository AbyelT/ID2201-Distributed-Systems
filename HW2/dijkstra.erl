-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).
-export([insert/2]).

%% returns the length of the shortest path to the
%% node or 0 if the node is not found.
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of 
        {Node, N, _} -> 
            %io:format("entry YES~n"),
            {distance, N};
        false -> 
            %io:format("entry NO~n"),
            0
    end.

%% replaces the entry for Node in Sorted with a new entry having a 
%% new length N and Gateway. 
replace(Node, N, Gateway, Sorted) ->
    NewSorted = lists:keydelete(Node, 1, Sorted),
    if 
        NewSorted == Sorted -> 
            %io:format("replace Sorted~n"),
            Sorted;
        true -> 
            %io:format("replace Unsorted: ~n"),
            insert({Node, N, Gateway}, NewSorted)
    end.


%% Sorts the given list in ascending order of the N
insert(A, []) -> 
    %io:format("Insert is done: ~p~n", [A]),
    [A];
insert({_, N, _}=A, [{_, N2, _}=B|Rest]=List) ->
    if 
        N > N2 ->
            %io:format("~b is BIGGER than ~b~n", [N, N2]),
            [B|insert(A, Rest)];
        true ->
            %io:format("~b is SMALLER than ~w~n", [N, N2]),
            [A|List]
    end.

%% update the list Sorted given that Node can be reached in N hops using Gateway.
%% If no entry is found then no new entry is added. 
%% The existing entry is replaced If the given entry has a better (shorter) path
update(Node, N, Gateway, Sorted) -> 
    case entry(Node, Sorted) of 
        0 -> 
            %io:format("already sorted~n"),
            Sorted;
        {distance, Len} ->
            if
                N < Len -> 
                    %io:format("~b is less than ~w~n", [N, Len]),
                    replace(Node, N, Gateway, Sorted);
                true -> 
                    %io:format("~b is bigger/equal than ~b~n", [N, Len]),
                    Sorted
            end
    end.

%%  construct a table given a sorted list
%%  of nodes, a map and a table constructed so far
iterate([], _, Table) ->
    Table;
iterate([{_, 'inf', _}|_], _, Table) ->
    Table;
iterate([{Node, N, Gateway}|Rest], Map, Table) ->
    NewSorted = findNewEntries(Node, N, Gateway, Map, Rest),    % add all reachable nodes to the sorted list 
    ExtendTable = [{Node, Gateway}|Table],                      % add the node, gateway to the table
    iterate(NewSorted, Map, ExtendTable).

%% adds all nodes from the given list into the Sorted list
findNewEntries([], _, _, _, Sorted) ->
    %io:format("done: ~p~n", [Sorted]),
    Sorted;
findNewEntries(Node, N, Gateway, Map, Sorted) ->
    case map:reachable(Node, Map) of
        [] -> 
            %io:format("no nearby for ~p~n", [Node]),
            update(Node, N, Gateway, Sorted);
        [First|Rest] -> 
            %io:format("found nearby nodes for ~s~n", [Node]),
            AddedElem = findNewEntries(First, N+1, Gateway, Map, Sorted), %%updatera map?
            findNewEntries(Rest, N, Gateway, Map, AddedElem)
    end.

%% creates a routing table
table(Gateways, Map) -> 
    AllNodes = map:all_nodes(Map),
    InitialSorted = initialiseSort(Gateways, AllNodes),
    iterate(InitialSorted, Map, []).

%% initializes a sorted list
initialiseSort(Gateways, AllNodes) ->
    Dummy = lists:map(fun(X) -> {X, inf, unknown} end, AllNodes),
    Sorted = lists:map(fun(X) -> {X, 0, X} end, Gateways),
    %io:format("~w~n", [Sorted ++ Dummy]),
    Sorted ++ Dummy.

route(Node, Table) -> 
    case lists:keyfind(Node, 1, Table) of 
        {Node, Gateway} -> {ok, Gateway};
        false -> notfound
    end.