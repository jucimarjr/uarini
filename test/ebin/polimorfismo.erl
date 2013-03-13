-module(polimorfismo).

-export([imprimir_imposto/1, main/0, new_/0]).

new_() ->
    ObjectID = ooe:new([]), {polimorfismo, ObjectID}.

imprimir_imposto(IPVA) ->
    io:format("Ipva: ~p~n",
	      [(erlang:element(1,
			       IPVA)):calcularIpva(erlang:element(2, IPVA))]).

main() ->
    Caminhao = caminhao:constructor(1995, "JXT - 1234", 3),
    Bicicleta = bicicleta:constructor(1995, "JXT - 1234",
				      3),
    Carro = carro:constructor(2008, "MNO - 4321"),
    imprimir_imposto(Caminhao),
    imprimir_imposto(Bicicleta),
    imprimir_imposto(Carro).
