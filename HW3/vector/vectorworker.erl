-module(vectorworker).

-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter, Nodes) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, Nodes) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, Nodes) ->
    random:seed(Seed, Seed, Seed),
        receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, vector:zero(Nodes)); %% vectors
        stop ->
        ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, InternalTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            {_, NewTime} = vector:inc(Name, vector:merge(Time, InternalTime)),  %% increment vector
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, InternalTime, {error, Error}}
        after Wait ->
            Selected = select(Peers),
            Message = {hello, random:uniform(100)},    
            {_, NewTime} = vector:inc(Name, InternalTime),                    %% increment time

            Selected ! {msg, NewTime, Message},
            jitter(Jitter),
            Log ! {log, Name, NewTime, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime)
        end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
        