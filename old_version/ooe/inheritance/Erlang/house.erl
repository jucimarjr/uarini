-module(house).
-export([constructor/1, main/1]).

constructor(ObjectID) ->
	ObjectID.

main(ObjectID) ->
	
	Cor1 = cor:constructor(make_ref(), 255, 127, 0),
	Animal = animal:constructor(make_ref(), "Animal", 10, Cor1),
	
	io:format("Componente vermelha da cor do animal: ~p~n", [cor:get_r((animal:get_cor(Animal)))]),
	io:format("Componente verde da cor do animal: ~p~n", [cor:get_g((animal:get_cor(Animal)))]),
	io:format("Componente azul da cor do animal: ~p~n", [cor:get_b((animal:get_cor(Animal)))]),
	
	Cor2 = cor:constructor(make_ref(), 200, 120, 167),
	Dog = dog:constructor(make_ref(), "Cachorro", 5, Cor2),
	
	io:format("Componente vermelha da cor do cachorro: ~p~n", [cor:get_r((dog:get_cor(Dog)))]),
	io:format("Componente verde da cor do cachorro: ~p~n", [cor:get_g((dog:get_cor(Dog)))]),
	io:format("Componente azul da cor do cachorro: ~p~n", [cor:get_b((dog:get_cor(Dog)))]),
	
	Cor3 = cor:constructor(make_ref(), 128, 87, 12),
	cor:set_r(Cor3, 10),

	dog:set_cor(Dog, Cor3),
	
	io:format("Nova componente vermelha da cor do cachoro: ~p~n", [cor:get_r((dog:get_cor(Dog)))]),
	io:format("Nova componente verde da cor do cachorro: ~p~n", [cor:get_g((dog:get_cor(Dog)))]),
	io:format("Nova componente azul da cor do cachorro: ~p~n", [cor:get_b((dog:get_cor(Dog)))]),
	
	io:format("Nome do animal: ~p~n", [animal:get_nome(Animal)]),
	io:format("Nome do cachorro: ~p~n", [dog:get_nome(Dog)]),
	
	io:format("Idade do animal: ~p~n", [animal:get_idade(Animal)]),
	io:format("Idade do cachorro: ~p~n", [dog:get_idade(Dog)]),
	
	animal:set_nome(Animal, "Animal com nome mudado"),
	dog:set_nome(Dog, "Cachorro com nome mudado"),
	
	io:format("Nome do animal: ~p~n", [animal:get_nome(Animal)]),
	io:format("Nome do cachorro: ~p~n", [dog:get_nome(Dog)]),
	
	animal:falar(Animal),
	dog:falar(Dog).
