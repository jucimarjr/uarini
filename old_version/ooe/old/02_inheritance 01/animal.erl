-module(animal, [Name, Color, Age]).
-export([constructor/0, print/1]).

constructor() ->
	Key = key(),
	put({animal, name, Key}, Name),
	put({animal, color, Key}, Color),
	put({animal, age, Key}, Age),
	Key.

print({Key, self}) ->
	io:format("I'm ~p~n", [get({animal, name, Key})]).

key() ->
	case get({animal, key}) of
		undefined ->
			put({animal, key}, 0),
			0;
		Key ->
			put({animal, key}, Key + 1),
			Key + 1
	end.
