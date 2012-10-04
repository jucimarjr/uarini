-module(dog, []).
-export([constructor/0, run/2, talk/1, run/1]).

constructor() ->
	Key = key(),
	Key.

run(Ball, Key)->
	io:format("Hunting ~p ball ~n", [Ball]).

talk(Key)-> 
	io:format("Woof! Woof!~n").

run(Key) ->
	io:format("Running~n").

key() ->
	case get({dog, key}) of
		undefined ->
			put({dog, key}, 0),
			0;
		Key ->
			put({dog, key}, Key + 1),
			Key + 1
	end.
