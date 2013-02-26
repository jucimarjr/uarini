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
-export([handle_error/3, print_errors/2, check_interface/1]).

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
	{"Method ~p/~p in class ~p is not public", [FuncName, Arity, ClassName]};
get_error_text(9, [Name, Arity, Modifiers, ClassName]) ->
	{"Method ~p/~p (~p) not found in class ~p, but required by its implemented "
		"interface", [Name, Arity, Modifiers, ClassName]}.

print_errors(_, []) ->
	ok;
print_errors(ClassName, [ {Line, Code, Args} | Rest ]) ->
	{Format, PArgs} = get_error_text(Code, Args),
	case Line of
		null ->
			io:format("class ~p - " ++ Format ++ "\n", [ClassName] ++ PArgs);
		_ ->
			io:format("class ~p:#~p - " ++ Format ++ "\n",
						[ClassName, Line] ++ PArgs)
	end,
	print_errors(ClassName, Rest).

%%----------------------------------------------------------------------------
%% Checa se a classe implementa alguma interface e atende a todos os
%% requisitos definidos na interface, gerando erros caso contrário
check_interface(ClassName) ->
	case st:get_interface(ClassName) of
		null ->
			ok;

		InterfaceName ->
			InterAllMethods = st:get_methods_with_parent_2(InterfaceName),
			ClassAllMethods = st:get_methods_with_parent_2(ClassName),

			check_interface(ClassName, InterAllMethods, ClassAllMethods)
	end.

check_interface(_, [], _) -> ok;
check_interface(ClassName, [Method | InterMethods], ClassMethods) ->
	case helpers:has_element(Method, ClassMethods) of
		true ->
			check_interface(ClassName, InterMethods, ClassMethods);

		false ->
			{{Name, Arity}, Modifiers} = Method,
			handle_error(null, 9, [Name, Arity, Modifiers, ClassName]),
			check_interface(ClassName, InterMethods, ClassMethods)
	end.
