-module(horista, []).
-export([getSalario/1, calcular/1]).
-export([constructor/0, start/0]).

constructor() ->
	Key = key(),
	Key.

getSalario({Key, super}) ->
	-1;

getSalario({Key, self}) ->
	-2.

calcular(Key) ->
	io:format("Salario da superclasse: ~p~n", [getSalario({Key, super})]),
	io:format("Salario da subclasse: ~p~n", [getSalario({Key, self})]).

start() ->
	spawn(fun() -> main() end).

main() ->	
	receive
		start -> ok
	end,

	Horista = horista:new(),
	Horista_key = Horista:constructor(),
	Horista:calcular(Horista_key).

key() ->
	case get({horista, key}) of
		undefined ->
			put({horista, key}, 0),
			0;
		Key ->
			put({horista, key}, Key + 1),
			Key + 1
	end.
