-module(aviao).

-export([exibeDados/1]).

-extends(transporte).

exibeDados(ObjectID) ->
    io:format("Aviao: Voando... ~n").

