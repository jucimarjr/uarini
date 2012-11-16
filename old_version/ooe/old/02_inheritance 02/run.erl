-module(run).
-export([run/0]).

run() ->
	compile:file("empregado.erl"),
	compile:file("horista.erl"),
	
	M = horista:new(),
	M_Key = M:constructor(),

	P = M:start(),
	P ! start.
