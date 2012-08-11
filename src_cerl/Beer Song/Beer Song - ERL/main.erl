-module(main).
-compile(export_all).

start_class() ->
	ooe:def_class(main),
	ooe:def_import(main, cantor),
	ooe:def_method(main, {sing, 2}, fun(A, B) -> sing(A, B) end).

bottles(Bottles) -> lists:reverse(lists:seq(0, Bottles)).

sing(Singers, Bottles) ->
	Cantor = ooe:constructor(cantor, {new, 1}, []),
	ooe:method(cantor, {spawn_singer, 2}, [Bottles], Cantor),
	%%lists:foreach(fun main:cantar/1, bottles(Bottles)),
	
	Cantor2 = ooe:constructor(cantor, {new, 1}, []),
	ooe:method(cantor, {sing_verse, 2}, [Singers], Cantor2).

cantar(Bottles) -> 
	Cantor = ooe:constructor(cantor, {new, 1}, []),
	ooe:method(cantor, {spawn_singer, 2}, [Bottles], Cantor).
