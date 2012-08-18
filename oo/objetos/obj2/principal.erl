-module(principal, []).
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

	Bola1 = bola:new("Azul", "12", "Borracha"), 
	Bola1_key = Bola1:constructor(),
	
	Bola2 = bola:new("Vermelha", "7", "Plástico"),
	Bola2_key = Bola2:constructor(),
	
	Cor1 = Bola1:get_cor(Bola1_key),
	Cor2 = Bola2:get_cor(Bola2_key),
	
	io:format("~p~n", [Cor1]),
	io:format("~p~n", [Cor2]).

key() ->
	case get({principal, key}) of
		undefined ->
			put({principal, key}, 0),
			0;
		Key ->
			put({principal, key}, Key + 1),
			Key + 1
	end.
