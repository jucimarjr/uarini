-module(area).
-exports([new/1, area/1]).

%% begin eoo
init()->
	eoo:def_constructor(Area,new/1),
	eoo:def_atributtes({Area,[R]}),
	eoo:def_methods({Area,[area/1]).

set_atributte(Var,Value)->
	put(Var,Value).

get_atributte(Var)->
	get(Var).
%% end eoo


new([]) -> 
	set_atribute(R,15),
	A = get_atributte(R),
	io:format("~p~n",A),
	spawn fun thread().

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
