-module(dog).
-export([constructor/4, get_idade/1, get_nome/1, get_cor/1, set_idade/2, set_nome/2, set_cor/2, falar/1]).

constructor(ObjectID, Nome, Idade, Cor) ->
	animal:constructor(ObjectID, Nome, Idade, Cor),
	ObjectID.

get_nome(ObjectID) ->
	animal:get_nome(ObjectID).

get_idade(ObjectID) ->
	animal:get_idade(ObjectID).

get_cor(ObjectID) ->
	animal:get_cor(ObjectID).

set_nome(ObjectID, Nome) ->
	animal:set_nome(ObjectID, Nome).

set_idade(ObjectID, Idade) ->
	animal:set_idade(ObjectID, Idade).

set_cor(ObjectID, Cor) ->
	animal:set_cor(ObjectID, Cor).

falar(ObjectID) ->	
	io:format("O cachorro está latindo.~n"),
	animal:falar(ObjectID).
