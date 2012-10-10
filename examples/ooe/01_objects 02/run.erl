-module(run).
-export([run/0]).

run() ->
	compile:file("gerenciadorDePessoas.erl"),
	compile:file("pessoa.erl"),
	M = gerenciadorDePessoas:new(),
	M_Key = M:constructor(),
	P = M:start(),
	P ! start.
