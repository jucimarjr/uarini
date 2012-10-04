-module(main).
-compile(export_all).


start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_method(?MODULE, {start, 1}, fun(A) -> start(A) end),
	ooe:def_method(?MODULE, {main, 1}, fun(A) -> main(A) end).

-define(RING, 12).

start(Token) ->
	H = lists:foldl(fun(Id,Pid)-> 
						ooe:constructor(nodo, {new, 2}, [Id, Pid]) 
					end, 
      				self(), 
      				lists:seq(?RING, 2, -1)
				),
	io:fwrite("inicio: ~p ~p ~n", [H,Token]),
	H ! Token,
	io:format("Definido processo ~p com parâmetros ~p ~p~n", [self(), 1, H]),
	ooe:constructor(nodo, {new, 3}, [1, H, self]).

main(Token) ->
	io:format("~w~n", [self()]),
	start(Token).
