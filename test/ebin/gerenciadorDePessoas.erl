-module(gerenciadorDePessoas).

-export([main/0, new_/0]).

new_() ->
    ObjectID = ooe:new([]),
    {gerenciadorDePessoas, ObjectID}.

main() ->
    PVitor = pessoa:constructor("Vitor Fernando Pamplona",
				"07/11/1983"),
    (erlang:element(1, PVitor)):receber(erlang:element(2,
						       PVitor),
					1.0e+3),
    PJoao = pessoa:constructor("João da Silva",
			       "18/02/1970"),
    (erlang:element(1, PJoao)):receber(erlang:element(2,
						      PJoao),
				       5.0e+2),
    (erlang:element(1, PJoao)):gastar(erlang:element(2,
						     PJoao),
				      1.0e+2).
