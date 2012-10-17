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

	Fab1 = fabricante:new("Nike", {08, 2012}),
	Fab1_key = Fab1:constructor(),

	Bola1 = bola:new("Azul", "12", "Borracha", {fabricante, Fab1, Fab1_key}),
	Bola1_key = Bola1:constructor(),

	Fab2 = fabricante:new("Adidas", {09, 2011}),
	Fab2_key = Fab2:constructor(),

	Bola2 = bola:new("Vermelha", "7", "Plástico", {fabricante, Fab2, Fab2_key}),
	Bola2_key = Bola2:constructor(),
	
	Cor1 = Bola1:get_cor({Bola1_key, self}),
	Cor2 = Bola2:get_cor({Bola2_key, self}),
	
	io:format("~p~n", [Cor1]),
	io:format("~p~n", [Cor2]),

	Bola1:set_cor("Amarela", {Bola1_key, self}),
	Bola2:set_cor("Preta", {Bola2_key, self}),

	Cor3 = Bola1:get_cor({Bola1_key, self}),
	Cor4 = Bola2:get_cor({Bola2_key, self}),

	io:format("~p~n", [Cor3]),
	io:format("~p~n", [Cor4]),

	Fab1_nome = Bola1:get_fabricante_nome({Bola1_key, self}),
	Fab2_nome = Bola2:get_fabricante_nome({Bola2_key, self}),
	
	io:format("~p~n", [Fab1_nome]),
	io:format("~p~n", [Fab2_nome]),
	
	F1_nome = Fab1:get_nome({Fab1_key, self}),
	F2_nome = Fab2:get_nome({Fab2_key, self}),
	
	io:format("~p~n", [F1_nome]),
	io:format("~p~n", [F2_nome]),

	Fab1_data = Bola1:get_fabricante_data_fabricacao({Bola1_key, self}),
	Fab2_data = Bola2:get_fabricante_data_fabricacao({Bola2_key, self}),

	io:format("~p~n", [Fab1_data]),
	io:format("~p~n", [Fab2_data]),

	Bola1:set_fabricante_nome("Topper", {Bola1_key, self}),
	Bola2:set_fabricante_nome("Umbro", 	{Bola2_key, self}),

	Fab3_nome = Bola1:get_fabricante_nome({Bola1_key, self}),
	Fab4_nome = Bola2:get_fabricante_nome({Bola2_key, self}),
	
	io:format("~p~n", [Fab3_nome]),
	io:format("~p~n", [Fab4_nome]),

	{Fab5_nome_fabricante, Fab5_nome_fabricante_key} = Bola1:get_fabricante({Bola1_key, self}),
	Fab5_nome = Fab5_nome_fabricante:get_nome({Fab5_nome_fabricante_key, self}),
	io:format("~p~n", [Fab5_nome]).
 
key() ->
	case get({principal, key}) of
		undefined ->
			put({principal, key}, 0),
			0;
		Key ->
			put({principal, key}, Key + 1),
			Key + 1
	end.
