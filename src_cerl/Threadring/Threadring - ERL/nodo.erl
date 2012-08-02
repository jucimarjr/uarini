-module(nodo).
-compile(export_all).

start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_constructor(?MODULE, {new, 2}, fun(A, B) -> new(A, B) end),
	ooe:def_constructor(?MODULE, {new, 3}, fun(A, B, C) -> new(A, B, C) end),
	ooe:def_method(?MODULE, {roundtrip, 2}, fun(A, B) -> roundtrip(A, B) end).

new(Id, Pid) ->
	spawn(?MODULE, roundtrip, [Id, Pid]).

new(Id, Pid, self) ->
	roundtrip(Id, Pid).

roundtrip(Id, Pid) ->
	receive
		1 ->
			io:fwrite("fim: ~b ~p 1 ~n", [Id, Pid]),
	        erlang:halt();
      Token ->
			io:fwrite("~b ~p ~p ~n", [Id, Pid, Token-1]),
			Pid ! Token - 1,
			roundtrip(Id, Pid)
	end.
