-module(hist).
-export([new/1, update/3]).

%% creates a new history TEMP
new(Name) -> 
    [{0, Name}].

%% updates the current history
%% assume history is a list of tuples: {msgNr, from} TEMP
update(Node, N, History) ->
    %io:format("update history: ~w, ~w, ~w~n", [Node, N, History]),
    case lists:keyfind(Node, 2, History) of
        false -> 
            {new, [{N, Node}|History]};
        {MsgNr, _} -> 
            if 
                N > MsgNr -> 
                    Updated = [{N, Node}|History],
                    {new, Updated};
                true -> old
            end
    end.
    