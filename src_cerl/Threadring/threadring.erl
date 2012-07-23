-module(threadring).
-export([main/1, roundtrip/2]).

-define(RING, 12).

start(Token) ->
	H = lists:foldl(fun(Id,Pid)->
						A = spawn(threadring, roundtrip, [Id, Pid]), 
						io:format("Criado processo ~p com parâmetros ~p ~p~n",
									[A, Id, Pid]), 
						A 
					end, 
      				self(), 
      				lists:seq(?RING, 2, -1)
				),
	io:fwrite("inicio: ~p ~p ~n", [H,Token]),
	H ! Token,
	io:format("Definido processo ~p com parâmetros ~p ~p~n", [self(), 1, H]),
	roundtrip(1, H).

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

main(Token) ->
	io:format("~w~n", [self()]),
	start(Token).
