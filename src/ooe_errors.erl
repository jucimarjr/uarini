%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique Braz Aquino ( dhbaquino@gmail.com )
%%			Eden Freitas Ramos ( edenstark@gmail.com )
%%			Helder Cunha Batista ( hcb007@gmail.com )
%%			Josiane Rodrigues da Silva ( josi.enge@gmail.com )
%%			Lídia Lizziane Serejo de Carvalho ( lidializz@gmail.com )
%%			Rodrigo Barros Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Tratar as ocorrências de exceções

-module(ooe_errors).
-export([handle_error/3, print_errors/2, check_interface/1, check_guard/1]).

handle_error(Line, Code, Args) ->
	st:put_error(Line, Code, Args),
	error.

get_error_text(1, []) -> {"Using self variable on static context", []};
get_error_text(2, []) -> {"Using super on class that has no superclass", []};
get_error_text(3, []) -> {"Using super variable on static context", []};
get_error_text(4, []) -> {"Using super variable to call static method, should"
							 "use class name instead", []};
get_error_text(5, []) -> {"Calling object method on static context", []};
get_error_text(6, [ClassName]) -> {"Class '~p' not found", [ClassName]};
get_error_text(7, [ClassName, FuncName, Arity]) ->
	{"Method ~p/~p in class ~p is not static", [FuncName, Arity, ClassName]};
get_error_text(8, [ClassName, FuncName, Arity]) ->
	{"Method ~p/~p in class ~p is not public", [FuncName, Arity, ClassName]};
get_error_text(9, [Name, Arity, Modifiers, ClassName]) ->
	{"Method ~p/~p (~p) not found in class ~p, but required by one of its "
		"implemented interfaces", [Name, Arity, Modifiers, ClassName]};
get_error_text(10, [Name]) ->
	{"'~p' should be a variable", [Name]};
get_error_text(11, [Name, Class]) ->
	{"Method '~p' does not exist in class '~p'", [Name, Class]};
get_error_text(12, []) ->
	{"bad interface", []};
get_error_text(13, []) ->
	{"class attributes not implemented yet", []};
get_error_text(14, []) ->
	{"ooe element is illegal in guard", []}.

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
	case st:get_interface_list(ClassName) of
		null ->
			ok;

		InterfaceNameList ->
			InterAllMethodsTemp =
				[st:get_methods_with_parent_2(Name) || Name <- InterfaceNameList],
			InterAllMethods = helpers:flatten_norep(InterAllMethodsTemp),
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

%%----------------------------------------------------------------------------
%% checa se determinado guard é válido
check_guard({oo_remote, Ln,_,_}) ->
	handle_error(Ln, 14, []);

check_guard({call, _, {oo_remote, Ln,_,_}, _}) ->
	handle_error(Ln, 14, []);

check_guard({tuple, _, Elements}) ->
	check_guard(Elements);

check_guard({op, _, _, LeftExpr, RightExpr}) ->
	check_guard(LeftExpr),
	check_guard(RightExpr);

check_guard({cons, _Line, Element, Tail}) ->
	check_guard(Element),
	check_guard(Tail);

check_guard({op, _Line, _Op, RightExpr}) ->
	check_guard(RightExpr);

check_guard(_) ->
	ok.
