-module(main).
-compile(export_all).

start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_method(?MODULE, {run, 2}, fun(A, B) -> run(A, B) end),
	ooe:def_method(?MODULE, {ping, 3}, fun(A, B, C) -> ping(A, B, C) end).
	
run(R, D) ->
	Pong = ooe:constructor(pingpong, {new, 0}, []),
	ping(R, D, Pong).

ping(0, _, _) ->
    done;
ping(R, D, Pong) ->
    Pong ! {self(), D},
    receive
        {Pong, D} ->
            ping(R - 1, D, Pong)
    end.
