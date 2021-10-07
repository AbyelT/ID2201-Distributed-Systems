-module(newworker).

-export([start/5, stop/1, peers/2]).

%% logical time
start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

% logical time
init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
        receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero()); %% logical time
        stop ->
        ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

%% added InternalTime to keep track of logical time
loop(Name, Log, Peers, Sleep, Jitter, InternalTime)->
    Wait = random:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            {_, NewTime} = time:inc(Name, time:merge(Time, InternalTime)),  %% increment time
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, InternalTime, {error, Error}}
        after Wait ->
            Selected = select(Peers),
            Message = {hello, random:uniform(100)},    
            {_, NewTime} = time:inc(Name, InternalTime),                    %% increment time

            Selected ! {msg, NewTime, Message},
            jitter(Jitter),
            Log ! {log, Name, NewTime, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTime)
        end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
        