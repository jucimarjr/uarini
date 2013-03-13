-module(navio).

-export([exibeDados/1, new_/0]).

new_() -> ObjectID = ooe:new([]), {navio, ObjectID}.

exibeDados(ObjectID) ->
    io:format("Navio: Titanic navegando... ~n").
