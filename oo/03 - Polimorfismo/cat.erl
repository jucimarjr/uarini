-module(cat, []).
-export([constructor/0, talk/1]).

constructor() ->
	Key = key(),
	Key.

talk(Key)-> 
	io:format("Meow! Meow!~n").

key() ->
	case get({dog, key}) of
		undefined ->
			put({cat, key}, 0),
			0;
		Key ->
			put({cat, key}, Key + 1),
			Key + 1
	end.
