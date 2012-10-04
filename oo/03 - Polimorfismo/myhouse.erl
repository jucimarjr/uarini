-module(myhouse, []).
-export([start/0, constructor/0, talkPet/1, main/0]).

constructor() ->
	Key = key(),
	Key.

talkPet({Type, Function, Key}) ->
	Function:talk(Key).

start() ->
	spawn(fun() -> main() end).

main() ->
	receive
		start -> ok
	end,

	M = myhouse:new(),
	M_key = M:constructor(),
		
	M:talkPet(
		begin
			Pet = pet:new(),
			Pet_key = Pet:constructor(),
			{pet, Pet, Pet_key}			
		end),

	M:talkPet(
		begin
			Dog = dog:new(),
			Dog_key = Dog:constructor(),
			{dog, Dog, Dog_key}
		end),

	M:talkPet(
		begin
			Cat = cat:new(),
			Cat_key = Cat:constructor(),
			{cat, Cat, Cat_key}
		end).

key() ->
	case get({myhouse, key}) of
		undefined ->
			put({myhouse, key}, 0),
			0;
		Key ->
			put({myhouse, key}, Key + 1),
			Key + 1
	end.
