-module(ping).
-compile(export_all).

start_class() ->
	ooe:def_class(?MODULE),
	ooe:def_constructor(?MODULE, {new, 1}, fun(A) -> new(A) end),
	ooe:def_method(?MODULE, {pingping, 1}, fun(A) -> pingping(A) end).	

new(Parameter) ->
	spawn(?MODULE, pingping, [Parameter]).

pingping(Data) ->
    receive
        {init, Dest, R} ->
            io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]),
			Dest ! {self(), R - 1},
            pingping(Data);
        {_From, R} when R =:= 0 ->
            io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]);
        {From, R} ->
			io:format("Mensagem: ~p. Numero de mensagens restantes: ~p~n", 
						[Data, R]),
            From ! {self(), R - 1},
            pingping(Data) 
    end.
