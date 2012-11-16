-module(run).
-export([run/0]).

run() ->
	compile:file("house.erl"),
	compile:file("animal.erl"),
	compile:file("dog.erl"),
	compile:file("cor.erl"),

	House = house:constructor(make_ref()),
	house:main(House).
