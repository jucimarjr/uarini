-module(empregado).

-export([calcular/1, constructor/0, get_salario/1]).

constructor() ->
    ObjectID = ooe:new([]), {empregado, ObjectID}.

get_salario(ObjectID) -> -1.

calcular(ObjectID) ->
    io:format("Salario da superclasse:~p ~n",
	      [get_salario(ObjectID)]).
