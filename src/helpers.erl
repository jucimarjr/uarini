%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Funcoes auxiliares para manipulacao de listas, entre outros

-module(helpers).
-export([has_element/2]).
%%-----------------------------------------------------------------------------
%% verifica se determinado elemento existe na lista
has_element(Element, [Element | _]) -> true;
has_element(Element, [_ | Rest]) -> has_element(Element, Rest);
has_element(_, []) -> false.
