-module(horista).

-export([calcular/1, get_salario/1, main/0, new/0]).

new() ->
    ObjectID = ooe:new([]), ok, {horista, ObjectID}.

get_salario(ObjectID) -> -2.

calcular(ObjectID) ->
    empregado:calcular(ObjectID),
    io:format("Salario da classe:~p ~n",
	      [get_salario(ObjectID)]).

main() ->
    H = horista:new(),
    (erlang:element(1, H)):get_salario(erlang:element(2,
						      H)),
    (erlang:element(1, H)):calcular(erlang:element(2, H)).
