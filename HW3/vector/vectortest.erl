-module(vectortest).
-export([run/2]).

%%report on your initial observations
run(Sleep, Jitter) ->
    Nodes = [john, paul, ringo, george],
    Log = vectorloggy:start(Nodes),
    A = vectorworker:start(john, Log, 13, Sleep, Jitter, Nodes),
    B = vectorworker:start(paul, Log, 23, Sleep, Jitter, Nodes),
    C = vectorworker:start(ringo, Log, 36, Sleep, Jitter, Nodes),
    D = vectorworker:start(george, Log, 49, Sleep, Jitter, Nodes),
    vectorworker:peers(A, [B, C, D]),
    vectorworker:peers(B, [A, C, D]),
    vectorworker:peers(C, [A, B, D]),
    vectorworker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vectorloggy:stop(Log),
    vectorloggy:stop(A),
    vectorloggy:stop(B),
    vectorloggy:stop(C),
    vectorloggy:stop(D).
