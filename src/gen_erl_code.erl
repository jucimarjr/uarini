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
		match/3
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

%% clausula que ignora a transformacao (expressao ja esta em Erlang)
match_expr(Expression) ->
	Expression.

%%-----------------------------------------------------------------------------
%% atribuicao
transform_match(Line, LeftExpr, RightExpr) ->
	case LeftExpr of
		%% se for ooe, ignora...
		{oo_remote, _Line2, _Self, _Attrb} ->
			{nil, Line};
		_ ->
			Transformed_LeftExpr = transform_inner_expr(LeftExpr),
			Transformed_RightExpr = transform_inner_expr(RightExpr),
			match(Line, Transformed_LeftExpr, Transformed_RightExpr)
	end.

%%-----------------------------------------------------------------------------
%% percorre uma expressao transformando cada subelemento
%% falta definir todos os possíveis nós, no caso tuplas, listas e... oq mais?
%% para botar um fim na recursao

transform_inner_expr(Expr) -> Expr.

%% operacoes como +, -, ++, etc.
%transform_inner_expr({op, Line, Op, LeftExp, RightExp}) ->
%	{op, Line, Op, match_attr_expr(LeftExp), match_attr_expr(RightExp)};

%% exemplo de nó para dar fim a recursao:
% transform_inner_expr({var ... }) -> {var ... };

%% operacao unaria, como +1, -2...
%% transform_inner_expr({op, Line, Op, RightExp}) ->
%% 	{op, Line, Op, match_attr_expr(RightExp)};

%% formacao de lista, internamente toda lista eh assim: [El1 | [El2 |[El3]]]
%% transform_inner_expr({cons, Line, Element, Tail}) ->
%%  Transformed_Element = transform_inner_expr(Element),
%%  Transformed_Tail = transform_inner_expr(Tail),
%%  {cons, Line, Transformed_Element, Transformed_Tail}.
