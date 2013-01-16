%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : traduz as expressoes em Uarini para Erlang

-module(gen_erl_code).
-export([match_param/1, match_expr/1]).

-import(gen_ast,
	[
		match/3, rcall/4, integer/2, call/3
	]).

%%-----------------------------------------------------------------------------
%% Faz o match de uma expressao nos parametros de uma clausula de uma funcao
%% de Uarini para Erlang

%% clausula que ignora transformacao (parametro ja esta em Erlang)
match_param(Parameter) ->
	Parameter.

%%-----------------------------------------------------------------------------
%% Faz o match de uma expressao no corpo de uma clausula de uma funcao
%% de Uarini para Erlang

match_expr({match, Line, LeftExpr, RightExpr}) ->
	transform_match(Line, LeftExpr, RightExpr);

%% chamadas de funcao
match_expr({call, _, _, _} = Expr) ->
	create_call(Expr);

%% clausula que ignora a transformacao (expressao ja esta em Erlang)
match_expr(Expression) ->
	Expression.

%%-----------------------------------------------------------------------------
%% atribuicao
transform_match(Ln1, LeftExpr, RightExpr) ->
	case LeftExpr of
		{oo_remote, Ln2, {atom, _, self}, {var, _, AttrName}} ->
			ObjectID_AST = gen_ast:var(Ln2, "ObjectID"),
			AttrNameAST = gen_ast:atom(Ln2, AttrName),
			AttrValueAST = transform_inner_expr(RightExpr),
			UpdateArgs = [ObjectID_AST, AttrNameAST, AttrValueAST],
			rcall(Ln2, ooe, update_attr, UpdateArgs);

		_ ->
			Transformed_LeftExpr = transform_inner_expr(LeftExpr),
			Transformed_RightExpr = transform_inner_expr(RightExpr),
			match(Ln1, Transformed_LeftExpr, Transformed_RightExpr)
	end.

%%-----------------------------------------------------------------------------
%% percorre uma expressao transformando cada subelemento
%% falta definir todos os possíveis nós, no caso tuplas, listas e... oq mais?
%% para botar um fim na recursao

transform_inner_expr({oo_remote, Ln1, ObjectVar, ObjectAttr}) ->
	ObjectVar2 = call(Ln1, element, [integer(Ln1, 2), ObjectVar]),
	{_, _, AttrName} = ObjectAttr,
	LookupArgs = [ObjectVar2, gen_ast:atom(Ln1, AttrName)],
	rcall(Ln1, ooe, lookup_attr, LookupArgs);

%% chamadas de funcao
transform_inner_expr({call, _, _, _} = Expr) ->
	create_call(Expr);

%% operacoes como +, -, ++, etc.
transform_inner_expr({op, Line, Op, LeftExp, RightExp}) ->
	{op, Line, Op, transform_inner_expr(LeftExp), transform_inner_expr(RightExp)};

%% operacao unaria, como +1, -2...
transform_inner_expr({op, Line, Op, RightExp}) ->
	{op, Line, Op, transform_inner_expr(RightExp)};

%% formacao de lista, internamente toda lista eh assim: [El1 | [El2 |[El3]]]
transform_inner_expr({cons, Line, Element, Tail}) ->
	Transformed_Element = transform_inner_expr(Element),
	Transformed_Tail = transform_inner_expr(Tail),
	{cons, Line, Transformed_Element, Transformed_Tail};

transform_inner_expr(Expr) -> Expr.

%%-----------------------------------------------------------------------------
%% funcao para transformar chamada de funcoes
create_call({call, Ln1, FuncLocation, ArgList}) ->
	TransfFuncLocation = create_func_loc(FuncLocation, ArgList),
	TransfArgList = [transform_inner_expr(Arg) || Arg <- ArgList],
	{call, Ln1, TransfFuncLocation, TransfArgList}.

%% chamadas classe::funcao(Args)
create_func_loc({oo_remote, Ln2, ClassName, FuncName}, ArgList) ->
	{_, _, ClassName2} = ClassName,
	{_, _, FuncName2} = FuncName,
	case st:is_constructor({ClassName2, {FuncName2, length(ArgList)}}) of
		true ->
			{remote, Ln2, ClassName, FuncName}
	end;
	%		false ->

%% chamadas de funcao modulo:funcao(Args)
create_func_loc({remote, Ln2, Module, Function}, _ArgList) ->
	{remote, Ln2, Module, Function};

%% chamadas de funcao funcao(Args)
create_func_loc(FunctionName, _ArgList) ->
	FunctionName.
