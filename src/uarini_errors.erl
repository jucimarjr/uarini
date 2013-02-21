%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique Braz Aquino ( dhbaquino@gmail.com )
%%			Eden Freitas Ramos ( edenstark@gmail.com )
%%			Helder Cunha Batista ( hcb007@gmail.com )
%%			Josiane Rodrigues da Silva ( josi.enge@gmail.com )
%%			Lídia Lizziane Serejo de Carvalho ( lidializz@gmail.com )
%%			Rodrigo Barros Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Tratar as ocorrências de exceções

-module(uarini_errors).
-export([handle_error/3, print_errors/2]).

handle_error(Line, Code, Args) ->
	st:put_error(Line, Code, Args),
	error.

get_error_text(1, []) -> "Using self variable on static context";
get_error_text(2, []) -> "Using super on class that has no superclass";
get_error_text(3, []) -> "Using super variable on static context";
get_error_text(4, []) -> "Using super variable to call static method, should"
							 "use class name instead";
get_error_text(5, []) -> "Calling object method on static context";
get_error_text(6, [ClassName]) -> "Class '"++ClassName++"' not found";
get_error_text(7, [ClassName, FuncName, Arity]) ->
	{"Method ~p/~p in class ~p is not static", [FuncName, Arity, ClassName]};
get_error_text(8, [ClassName, FuncName, Arity]) ->
	{"Method ~p/~p in class ~p is not public", [FuncName, Arity, ClassName]}.

print_errors(_, []) ->
	ok;
print_errors(ClassName, [ {Line, Code, Args} | Rest ]) ->
	{Format, PArgs} = get_error_text(Code, Args),
	io:format("class ~p:#~p - " ++ Format ++ "\n", [ClassName, Line] ++ PArgs),
	print_errors(ClassName, Rest).
