-module(aviao).

-export([exibeDados/1, new_/0]).

new_() -> ObjectID = ooe:new([]), {aviao, ObjectID}.

exibeDados(ObjectID) ->
    io:format("Aviao: Voando... ~n").
