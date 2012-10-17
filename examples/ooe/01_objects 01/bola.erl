-module(bola, [Cor, Circun, Material, Fabricante]).
-export([constructor/0, get_cor/1, get_circun/1,  get_material/1,
			set_cor/2, set_circun/2,  set_material/2,
			get_fabricante_nome/1, get_fabricante_data_fabricacao/1,
			set_fabricante_nome/2, set_fabricante_data_fabricacao/2]).
-export([get_fabricante/1]).

constructor() ->
	Key = key(),
	put({bola, cor, Key}, Cor),
	put({bola, circunferencia, Key}, Circun),
	put({bola, material, Key}, Material),
	put({bola, fabricante, Key}, Fabricante),	
	Key.

get_cor({Key, self}) ->
	get({bola, cor, Key}).

get_circun({Key, self}) ->
	get({bola, circunferencia, Key}).

get_material({Key, self}) ->
	get({bola, material, Key}).

set_cor(Cor_set, {Key, self}) ->
	put({bola, cor, Key}, Cor_set).

set_circun(Circun_set, {Key, self}) ->
	put({bola, circunferencia, Key}, Circun_set).

set_material(Material_set, {Key, self}) ->
	put({bola, material, Key}, Material_set).

get_fabricante({Key, self}) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	{Fab, Fab_key}.

get_fabricante_nome({Key, self}) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:get_nome({Fab_key, self}).		
	
get_fabricante_data_fabricacao({Key, self}) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:get_data_fabricacao({Fab_key, self}).

set_fabricante_nome(Nome_set, {Key, self}) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:set_nome(Nome_set, {Fab_key, self}).

set_fabricante_data_fabricacao(Data_set, {Key, self}) ->
	{fabricante, Fab, Fab_key} = get({bola, fabricante, Key}),
	Fab:set_data_fabricacao(Data_set, {Fab_key, self}).

key() ->
	case get({bola, key}) of
		undefined ->
			put({bola, key}, 0),
			0;
		Key ->
			put({bola, key}, Key + 1),
			Key + 1
	end.
