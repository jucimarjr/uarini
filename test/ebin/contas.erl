-module(contas).

-export([main/0, mostrarCalculo/3, new_/0]).

new_() -> ObjectID = ooe:new([]), {contas, ObjectID}.

mostrarCalculo(OperacaoMatematica, X, Y) ->
    io:format("O resultado eh: ~p~n",
	      [(erlang:element(1,
			       OperacaoMatematica)):calcular(erlang:element(2,
									    OperacaoMatematica),
							     X, Y)]).

main() ->
    mostrarCalculo(soma:new_(), 5, 2),
    mostrarCalculo(subtracao:new_(), 5, 2).
