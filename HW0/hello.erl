-module(hello).
-export([print/1]).

print(X) -> 
    io:format("hello, my name is ~s~n", [X]).