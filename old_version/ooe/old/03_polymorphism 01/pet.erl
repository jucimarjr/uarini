-module(pet, []).
-export([constructor/0, run/1, talk/1]).

constructor() ->
	Key = key(),
	Key.

run({Key, self}) ->
	io:format("Running~n").

talk({Key, self}) ->
	io:format("yada-yada-yada~n").

key() ->
	case get({pet, key}) of
		undefined ->
			put({pet, key}, 0),
			0;
		Key ->
			put({pet, key}, Key + 1),
			Key + 1
	end.
