%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Funcoes auxiliares para manipulacao de listas, entre outros

-module(helpers).
-export([has_element/2, orddict_store_all/2, insert_replace/2,
		insert_replace_all/2, flatten_norep/1]).

%%-----------------------------------------------------------------------------
%% verifica se determinado elemento existe na lista
has_element(Element, [Element | _]) -> true;
has_element(Element, [_ | Rest]) -> has_element(Element, Rest);
has_element(_, []) -> false.

%%-----------------------------------------------------------------------------
%% 
orddict_store_all([], ODict) -> ODict;
orddict_store_all([ {Key, Value} | Rest], ODict) ->
	orddict_store_all(Rest, orddict:store(Key, Value, ODict)).

%%-----------------------------------------------------------------------------
%% insere um dado elemento em uma lista, mas, se já existir, ele "substitui"
insert_replace(Element, List) ->
	insert_replace(Element, List, []).

insert_replace(Element, [Element | Rest], List) ->
	lists:reverse(List, [Element | Rest]);
insert_replace(Element, [], List) ->
	lists:reverse(List, [Element]);
insert_replace(Element, [OtherElement | Rest], List) ->
	insert_replace(Element, Rest, [OtherElement | List]).

%%-----------------------------------------------------------------------------
%% aplica insert_replace para cada elemento da lista recebida
insert_replace_all([], List) -> List;
insert_replace_all([Element | Rest], List) ->
	NewList = insert_replace(Element, List),
	insert_replace_all(Rest, NewList).

%%-----------------------------------------------------------------------------
%% Recebe uma lista de listas, mescla os elementos de todas em uma única lista
%% sem repetir nenhum elemento
flatten_norep(List) ->
	FlattenedList = lists:flatten(List),
	insert_replace_all(FlattenedList, []).
