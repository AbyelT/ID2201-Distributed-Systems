-module(ping).
-export([respond/0]).
-export([request/1]).

request(X) ->
    io:format("ZOT start~n"),
    register(zot, self()),
    {bar, X} ! 'zot@DESKTOP-SAHUIOR',
    receive
        M ->     
            io:format("Message received from: ~w~n", [M]),
            io:format("Process ~s exits ~n", [zot])
    end.
    

respond() ->
    io:format("BAR start~n"),
    register(bar, self()),
    receive 
        X -> 
            io:format("incoming message from process: ~w~n", [X]),
            {zot, X} ! {pid, 'bar@DESKTOP-SAHUIOR'},
            io:format("Process ~s exits ~n", [foo]);
        {_,_} -> 
            error
    end.



    