-module(onibus).

-export([exibeDados/1, new_/0]).

new_() -> ObjectID = ooe:new([]), {onibus, ObjectID}.

exibeDados(ObjectID) ->
    io:format("Onibus: Indo no engarrafamento... ~n").
