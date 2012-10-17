-module(dog, []).
-export([constructor/0, run/1, run/2, talk/1]).

constructor() ->
	Key = key(),
	Key.

run({Key, self}) ->
	io:format("Running~n").

run(Ball, {Key, self})->
	io:format("Hunting ~p ball ~n", [Ball]).

talk({Key, super}) ->
	io:format("yada-yada-yada~n");

talk({Key, self})->
	io:format("Woof! Woof!~n").

key() ->
	case get({dog, key}) of
		undefined ->
			put({dog, key}, 0),
			0;
		Key ->
			put({dog, key}, Key + 1),
			Key + 1
	end.
