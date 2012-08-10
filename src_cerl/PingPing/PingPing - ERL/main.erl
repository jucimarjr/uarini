-module(main).
-compile(export_all).

start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_method(?MODULE, {run, 2}, fun(A, B) -> run(A, B) end).
	
run(R, D) ->
	P1 = ooe:constructor(ping, {new, 1}, [D]),
	P2 = ooe:constructor(ping, {new, 1}, [D]),
	P1 ! {init, P2, R},
	P2 ! {init, P1, R}.()
