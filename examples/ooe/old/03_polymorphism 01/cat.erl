-module(cat, []).
-export([constructor/0, run/1, talk/1]).

constructor() ->
	Key = key(),
	Key.

run({Key, self}) ->
	io:format("Running~n").

talk({Key, super}) ->
	io:format("yada-yada-yada~n");

talk({Key, self})-> 
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
