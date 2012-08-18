-module(run).
-export([run/0]).

run() ->
	compile:file("principal.erl"),
	compile:file("bola.erl"),
	M = principal:new(),
	M_Key = M:constructor(),
	P = M:start(),
	P ! start.
