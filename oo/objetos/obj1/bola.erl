-module(bola, [Cor, Circun, Material]).
-export([constructor/0, get_cor/1, get_circun/1,  get_material/1]).

constructor() ->
	Key = key(),
	put({bola, cor, Key}, Cor),
	put({bola, circunferencia, Key}, Circun),
	put({bola, material, Key}, Material),
	Key.

get_cor(Key) ->
	get({bola, cor, Key}).

get_circun(Key) ->
	get({bola, circunferencia, Key}).

get_material(Key) ->
	get({bola, material, Key}).

key() ->
	case get({bola, key}) of
		undefined ->
			put({bola, key}, 0),
			0;
		Key ->
			put({bola, key}, Key + 1),
			Key + 1
	end.
