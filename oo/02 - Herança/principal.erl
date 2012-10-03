-module(principal, []).
-export([constructor/0, start/0, main/0]).

constructor() ->
	Key = key(),
	Key.

start() ->
	spawn(fun() -> main() end).

main() ->	
	receive
		start -> ok
	end,

	Dog = dog:new("Milu", "White", 7),
	Dog_key = Dog:constructor(),

	Animal = animal:new("Tom", "Gray", 5),
	Animal_key = Animal:constructor(),

	Dog:bark(Dog_key),
	Dog:print(Dog_key),

	Animal:print(Animal_key).
	 
key() ->
	case get({principal, key}) of
		undefined ->
			put({principal, key}, 0),
			0;
		Key ->
			put({principal, key}, Key + 1),
			Key + 1
	end.
