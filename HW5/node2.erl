-module(node2).
-define(Stabilize, 1000).
-define(Timeout, 10000).

-export([start/1, start/2, status/1]).

%% starts the process, either as the first node 
%% in the ring as a connecting one
start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

%% Connects to the succesor, starts the stabilizer
%% and calls the message handler for node
init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    %io:format("~w START~n",[Id]),
    node(Id, Predecessor, Successor, storage:create()).

%% Either we are the first node in a ring
%% or we are trying to conenct to one
connect(Id, nil) ->
    io:format("self~n",[]),
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            io:format("in a group with peer~n",[]),
           {ok, {Skey, Peer}}
        after ?Timeout ->
            io:format("Time out: no response~n",[])
    end.

%% the node
node(Id, Predecessor, Successor, Store) ->
    receive
        % add key/value pair
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        % lookup key/value
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        % the node receives an handover of elements
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);
        % a peer wants to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        % a new Node notifies us it may be our predecessor
        {notify, New} ->
            {Pred, Store2} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store2);
        % a predecessor want to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        % our successor informs us about our predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        %used for probing
        probe ->
            io:format("probe start!~n"),        
            probe:create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            probe:remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            io:format("probe forward!~n"),        
            probe:forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);  
        % time to stabilise
        stabilize ->
            %io:format("stabilise: ~w~n", [Id]),  
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);    
        % stops the process
        stop ->
            ok;
        status -> 
            io:format("~w status: ~w, ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Store);    
        % catch all
        Error ->                                        
            io:format("strange message: ~w~n", [Error]),        
            node(Id, Predecessor, Successor, Store)
    end.

%% add a new key/value 
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

%% stämmer detta?
handover(Id, Store, Nkey, Npid) ->
    {Rest, Keep} = storage:split(Id, Nkey, Store), 
    Npid ! {handover, Rest},
    Keep.
        
%% sends a request message to its succesor, the next function
%% clause handles the response part of the succesor
stabilize({_, Spid}) ->
    Spid ! {request, self()}.
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->              % succesor does not know of the nodes existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->          % succesor is pointing to this node, it's alright
            Successor;
        {Skey, _} ->        % succesor is pointing to itself   
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->     % succesor has an another node as predecessor
            case key:between(Xkey, Id, Skey) of
                true ->     % the other node is between the node and the succesor
                    Xpid ! {notify, {Id, self()}},
                    {Xkey, Xpid};
                false ->    % the current node is between Xpid and Spid
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

%% sets up a timer that triggers the stabilize()
%% after a predenfined interval
schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

%% handles the request from a Peer node
%% by sending the status of its predecessor
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

%% handles notify messages by checking if the
%% new Node is our predecessor or not

%% stämmer detta?
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->              % we have no predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            %Npid ! {status, {Nkey, Npid}},  %% is this needed anyone??
            {{Nkey, Npid}, Keep};
        {Id, _} ->          % we are pointing to ourselves
            Keep = handover(Id, Store, Nkey, Npid),
            %Npid ! {status, {Nkey, Npid}},
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->        % we do have a predecessor
            case key:between(Nkey, Pkey, Id) of
                true ->     % the new Node is between our predecessor and us
                    Keep = handover(Id, Store, Nkey, Npid),
                    %Npid ! {status, {Nkey, Npid}},
                    {{Nkey, Npid}, Keep};
                false ->    % the new Node is between our predecessor and us
                    %Npid ! {status, Predecessor},
                    {Predecessor, Store}
            end
    end.

status(Pid) ->
    Pid ! status.