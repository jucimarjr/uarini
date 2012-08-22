-module(fabricante, [Nome, Data_Fabricacao]).
-export([constructor/0, get_nome/1, get_data_fabricacao/1,
			set_nome/2, set_data_fabricacao/2]).

constructor() ->
	Key = key(),
	put({fabricante, nome, Key}, 			Nome),
	put({fabricante, data_fabricacao, Key}, Data_Fabricacao),
	Key.

get_nome(Key) ->
	get({fabricante, nome, Key}).

get_data_fabricacao(Key) ->
	get({fabricante, data_fabricacao, Key}).

set_nome(Nome_set, Key) ->
	put({fabricante, nome, Key}, Nome_set).

set_data_fabricacao(Data_Fabricacao_set, Key) ->
	put({fabricante, data_fabricacao, Key}, Data_Fabricacao_set).

key() ->
	case get({fabricante, key}) of
		undefined ->
			put({fabricante, key}, 0),
			0;
		Key ->
			put({fabricante, key}, Key + 1),
			Key + 1
	end.	
