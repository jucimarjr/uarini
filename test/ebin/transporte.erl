-module(transporte).

-export([exibeDados/1, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {transporte, ObjectID}.

exibeDados(ObjectID) ->
    io:format("Transporte: Metodo para exibicao dos "
	      "dados: ~n").
