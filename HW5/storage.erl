-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

%% creates new store
create() ->
    [].

%% adds a key value to the store
add(Key, Value, Store) ->
    [{Key, Value}|Store].

%% lookup a key value in the store
lookup(Key, Store) ->
    case lists:keyfind(Key, 1, Store) of
        false -> false;
        Found -> Found
    end.

%% splits the store into one with keys inside the (From, To]
%% and outside it
split(From, To, Store) ->
    lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

%% merge two lists of key values 
merge(Entries, Store) -> 
    lists:merge(Entries, Store).
