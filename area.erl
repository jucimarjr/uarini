%%-----------------------------------------------------------------
%% exemplo de classe em erlang
%%-----------------------------------------------------------------

-class( area ).
-constructor(new/1).
-export([ area/1 ]).

-def_atributes([
	{R,0},
]).

new([]) -> spawn fun thread().

thread() ->
	receive
		{ From, { area, [Dados] } } ->
			From ! area(Dados),
			thread();
			
		{ From, Other } ->
			From ! { self(), { error, Other } },
			thread()
	end.

area(Dados)->
	ok.
