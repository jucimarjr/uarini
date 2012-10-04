-module(pingping).
-export([run/2]).


run(R, D) ->
    P1 = spawn(fun() -> pingping(D) end),
    P2 = spawn(fun() -> pingping(D) end),
    P1 ! {init, self(), P2, R},
    P2 ! {init, self(), P1, R}.
    
pingping(Data) ->
    receive
        {init, _From, Dest, R} ->
            io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]),
			Dest ! {self(), R - 1, _From},
            pingping(Data);
        {_From, R, _Parent} when R =:= 0 ->
            io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]);
        {_From, R, Parent} ->
			io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]),
            _From ! {self(), R - 1, Parent},
            pingping(Data) 
    end.
