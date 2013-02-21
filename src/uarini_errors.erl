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
	"Method "++FuncName++"/"++Arity++" in class "++ClassName++" is not static";
get_error_text(8, [ClassName, FuncName, Arity]) ->
	"Method "++FuncName++"/"++Arity++" in class "++ClassName++" is not public".

print_errors(_, []) ->
	ok;
print_errors(ClassName, [ {Line, Code, Args} | Rest ]) ->
	io:format("class ~p:#~p - ~s\n", [ClassName, Line, get_error_text(Code, Args)]),
	print_errors(ClassName, Rest).
