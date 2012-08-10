-module(run).
-export([run/0]).

run() ->
	compile:file("ooe.erl"),
	compile:file("main.erl"),
	compile:file("cantor.erl"),
	compile:file("caneco.erl"),	
	main:start_class(),
	cantor:start_class(),
	caneco:start_class(),
	main:sing(12, 15).
