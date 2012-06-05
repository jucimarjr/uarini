-module(area).
-export([new/1, area/1]).

%% ----------------------------------------------------------------------------
%% create by eoo - begin
%% ----------------------------------------------------------------------------
class_attributes()->
	put({a,R},nil),
	put({a,J}, 0.234),
	put({a,S},"teste"),
	put({a,K}, 4),
	put({a,Z},nil).

% gerado automaticamente para inicializar o objeto
class_init()->
	class_attributes().

%% ----------------------------------------------------------------------------
%% create by eoo - end
%% ----------------------------------------------------------------------------


%% para ser classe , deveria ter o new e o thread
%% seriam obrigatorias, mas seriam criadas pelo programador
new([]) ->
	class_init(), %% colocado como 1a linha do new
	spawn fun thread().

thread() ->
	receive
		{ From, { area, [Dados] } } ->
			From ! area(Dados),
			thread();
		
		{ From, { print, [] } } ->
			From ! print(),
			thread();
		
		{ From, Other } ->
			From ! { self(), { error, Other } },
			thread()
	end.

%% funcoes criadas pelo programador daqui para baixo
area(Dados)->
	ok.

print() -> 
	A = self::R.
