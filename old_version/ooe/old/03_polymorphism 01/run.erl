-module(run).
-export([run/0]).

run() ->
	compile:file("myhouse.erl"),
	compile:file("pet.erl"),
	compile:file("dog.erl"),
	compile:file("cat.erl"),
	
	M = myhouse:new(),
	M_Key = M:constructor(),

	P = M:start(),
	P ! start.
