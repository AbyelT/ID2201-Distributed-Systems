-module(node1).
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
    io:format("~w START~n",[Id]),
    node(Id, Predecessor, Successor).

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
node(Id, Predecessor, Successor) ->
    receive
        % a peer wants to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % a new Node notifies us it may be our predecessor
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % a predecessor want to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % our successor informs us about our predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        probe ->
            io:format("probe start!~n"),        
            probe:create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            probe:remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            io:format("probe forward!~n"),        
            probe:forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);  
        % time to stabilise
        stabilize ->
            %io:format("stabilise: ~w~n", [Id]),  
            stabilize(Successor),
            node(Id, Predecessor, Successor);    
        % stops the process
        stop ->
            ok;
        status -> 
            io:format("~w status: ~w, ~w~n",[Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor);    
        % catch all
        Error ->                                        
            io:format("strange message: ~w~n", [Error]),        
            node(Id, Predecessor, Successor)
    end.

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
                    stabilize(Pred, Id, {Xkey, Xpid});
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
notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->              % we have no predecessor
            Npid ! {status, {Nkey, Npid}},
            {Nkey, Npid};
        {Id, _} ->          % we are pointing to ourselves
            Npid ! {status, {Nkey, Npid}},
            {Nkey, Npid};
        {Pkey, _} ->        % we do have a predecessor
            case key:between(Nkey, Pkey, Id) of
                true ->     % the new Node is between our predecessor and us
                    Npid ! {status, {Nkey, Npid}},
                    {Nkey, Npid};
                false ->    % the new Node is between our predecessor and us
                    Npid ! {status, Predecessor},
                    Predecessor
            end
    end.

status(Pid) ->
    Pid ! status.