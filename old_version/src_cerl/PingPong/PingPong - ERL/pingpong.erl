-module(pingpong).
-compile(export_all).

start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_constructor(?MODULE, {new, 0}, fun() -> new() end),
	ooe:def_method(?MODULE, {pong, 0}, fun() -> pong() end).

new() ->
	spawn(?MODULE, pong, []).

pong() ->
    receive
        {From, D} ->
            From ! {self(), D},
            pong()
    end.
