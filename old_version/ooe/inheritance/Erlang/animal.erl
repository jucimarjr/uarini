-module(animal).
-export([constructor/4, get_nome/1, get_idade/1, get_cor/1, set_nome/2, set_idade/2, set_cor/2, falar/1]).

constructor(ObjectID, Nome, Idade, Cor) ->	 
	put({var_object, ObjectID, nome, private}, Nome),
	put({var_object, ObjectID, idade, private}, Idade),
	put({var_object, ObjectID, cor, private}, Cor),
	ObjectID.

get_nome(ObjectID) -> 
	get({var_object, ObjectID, nome, private}).

get_idade(ObjectID) -> 
	get({var_object, ObjectID, idade, private}).

get_cor(ObjectID) ->
	get({var_object, ObjectID, cor, private}).

set_nome(ObjectID, Nome) -> 
	put({var_object, ObjectID, nome, private}, Nome).

set_idade(ObjectID, Idade) -> 
	put({var_object, ObjectID, idade, private}, Idade).

set_cor(ObjectID, Cor) -> 
	put({var_object, ObjectID, cor, private}, Cor).

falar(ObjectID) ->
	io:format("O animal está falando.~n").
