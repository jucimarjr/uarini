-module(dog, [Name, Color, Age]).
-export([constructor/0, print/1, bark/1]).

constructor() ->
	Key = key(),
	put({dog, name, Key}, Name),
	put({dog, color, Key}, Color),
	put({dog, age, Key}, Age),
	Key.

print({Key, self}) ->
	io:format("I'm ~p~n", [get({dog, name, Key})]).

bark(Key)->
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
