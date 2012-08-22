-module(bola, [Cor, Circun, Material, Fabricante]).
-export([constructor/0, get_cor/1, get_circun/1,  get_material/1,
			set_cor/2, set_circun/2,  set_material/2,
			get_fabricante_nome/1, get_fabricante_data_fabricacao/1]).

constructor() ->
	Key = key(),
	put({bola, cor, Key}, Cor),
	put({bola, circunferencia, Key}, Circun),
	put({bola, material, Key}, Material),
	put({bola, fabricante, Key}, Fabricante),	
	Key.

get_cor(Key) ->
	get({bola, cor, Key}).

get_circun(Key) ->
	get({bola, circunferencia, Key}).

get_material(Key) ->
	get({bola, material, Key}).

set_cor(Cor_set, Key) ->
	put({bola, cor, Key}, Cor_set).

set_circun(Circun_set, Key) ->
	put({bola, circunferencia, Key}, Circun_set).

set_material(Material_set, Key) ->
	put({bola, material, Key}, Material_set).

get_fabricante_nome(Key) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:get_nome(Fab_key).		
	
get_fabricante_data_fabricacao(Key) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:get_data_fabricacao(Fab_key).

key() ->
	case get({bola, key}) of
		undefined ->
			put({bola, key}, 0),
			0;
		Key ->
			put({bola, key}, Key + 1),
			Key + 1
	end.
