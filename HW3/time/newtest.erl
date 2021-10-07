-module(newtest).
-export([run/2]).

%%report on your initial observations
run(Sleep, Jitter) ->
    Log = newloggy:start([john, paul, ringo, george]),
    A = newworker:start(john, Log, 13, Sleep, Jitter),
    B = newworker:start(paul, Log, 23, Sleep, Jitter),
    C = newworker:start(ringo, Log, 36, Sleep, Jitter),
    D = newworker:start(george, Log, 49, Sleep, Jitter),
    newworker:peers(A, [B, C, D]),
    newworker:peers(B, [A, C, D]),
    newworker:peers(C, [A, B, D]),
    newworker:peers(D, [A, B, C]),
    timer:sleep(5000),
    newloggy:stop(Log),
    newworker:stop(A),
    newworker:stop(B),
    newworker:stop(C),
    newworker:stop(D).
