-module(principal, []).
-compile([export_all]).

constructor() ->
	ok.

main() ->
	Bola1 = bola:new("Azul", "12", "Borracha"), 
	Bola1_key = Bola1:constructor(),
	
	Bola2 = bola:new("Vermelha", "7", "Plástico"),
	Bola2_key = Bola2:constructor(),
	
	Cor1 = Bola1:get_cor(Bola1_key),
	Cor2 = Bola2:get_cor(Bola2_key),
	
	io:format("~p~n", [Cor1]),
	io:format("~p~n", [Cor2]).
