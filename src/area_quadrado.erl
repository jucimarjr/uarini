%%-----------------------------------------------------------------
%% exemplo de classe em erlang
%% essa seria uma sb classe de area
%% herdaria todos os metodos e atributos. 
%% redefiniria a funcao area
%%-----------------------------------------------------------------

-class( area_quadrado ).
-extends( area ).

area(Raio)->
	Raio * Raio.
