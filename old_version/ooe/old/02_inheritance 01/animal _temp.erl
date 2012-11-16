-module(Animal, [{N, C, A}]).
-compile(export_all).

load_class() ->
		ooe:def_class(Animal),
		ooe:def_attribute(Animal, name),
		ooe:def_attribute(Animal, color),
		ooe:def_attribute(Animal, age),
		ooe:def_constructor(Animal, constructor, fun() -> constructor() end.).

constructor() ->
	Key = key(),
	put({animal, name, Key}, Name),
	put({animal, color, Key}, Color),
	put({animal, age, Key}, Age),
	Key.

print() ->
	io:format("I'm ~p~n", [self::name]).	 
