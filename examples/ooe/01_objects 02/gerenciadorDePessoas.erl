-module(gerenciadorDePessoas, []).
-export([constructor/0, start/0, main/0]).

constructor() ->
	Key = key(),
	Key.

start() ->
	spawn(fun() -> main() end).

main() ->	
	receive
		start -> ok
	end,

	PVitor = pessoa:new("Vitor Fernando Pamplona","07/11/1983"),
	PVitor_key = PVitor:constructor(),
	PVitor:receber(1000, PVitor_key),

	DinheiroVitor = PVitor:get_dinheiro(PVitor_key),
	io:format("Dinheiro na carteira de Vitor ~p~n", [DinheiroVitor]),

	PJoao = pessoa:new("João da Silva", "18/02/1970"),
	PJoao_key = PJoao:constructor(),
	PJoao:receber(500, PJoao_key),
	PJoao:gastar(500, PJoao_key),

	DinheiroJoao = PJoao:get_dinheiro(PJoao_key),
	io:format("Dinheiro na carteira de Joao ~p~n", [DinheiroJoao]).

key() ->
	case get({gerenciadorDePessoas, key}) of
		undefined ->
			put({gerenciadorDePessoas, key}, 0),
			0;
		Key ->
			put({gerenciadorDePessoas, key}, Key + 1),
			Key + 1
	end.
