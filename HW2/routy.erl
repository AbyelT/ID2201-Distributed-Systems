-module(routy).

-export([start/2, stop/1, router/6, init/1, status/1]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    % io:format("~w: running router again~n", [Name]),
    receive
        {links, Node, R, Links} ->
            io:format("~w: link received ~w, ~w, ~w~n", [Name, Node, R, Links]),
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    % io:format("~w: new link!~n", [Name]),
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    %% auto-update routing table with dijkstra
                        %% Table1 = dijkstra:table(intf:list(Intf), Map1), let Table -> Table1
                    router(Name, N, Hist1, Intf, Table, Map1); 
                old ->
                    % io:format("~w: old link~n", [Name]),
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        {route, Name, From, Message} ->
            io:format("~w: received message from ~w, ~s ~n", [Name, From, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~w: routing message (~s)~n", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                        end;
                notfound ->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);       
        {add, Node, Pid} ->
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            %% auto-broadcast every time a link is added
                %% Message = {links, Name, N, intf:list(Intf1)},
                %% intf:broadcast(Message, Intf1),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        {status, From} ->
            io:format("~w: send received from ~w~n", [Name, From]),
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        stop ->
            ok
    end.

%% given a name, finds the process/reference then sends a 'status' message and display the result
status(Ref) ->
    Ref ! {status, self()},
        receive
            {status, From} -> io:format("status from ~w~n", [From])
    end.
