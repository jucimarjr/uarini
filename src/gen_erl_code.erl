%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao3201;0c
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : traduz as expressoes em Uarini para Erlang

-module(gen_erl_code).
-export([match_param/1, match_expr/1]).

-import(gen_ast,
	[
		match/3, rcall/4, integer/2, call/3, var/2, atom/2
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

%% funs fun() -> ... end
match_expr({'fun', Ln, {clauses, ClauseList}}) ->
	transform_fun(Ln, ClauseList);

%% acesso a atributos
match_expr({oo_remote, _, _, _} = Expr) ->
	transform_inner_expr(Expr);

%% envio de mensagem
match_expr({op, Ln, '!', LeftExpr, RightExpr}) ->
	{op, Ln, '!', transform_inner_expr(LeftExpr), transform_inner_expr(RightExpr)};

%% clausula que ignora a transformacao (expressao ja esta em Erlang)
match_expr(Expression) ->
	Expression.

%%-----------------------------------------------------------------------------
%% atribuicao
transform_match(Ln1, LeftExpr, RightExpr) ->
	Scope = st:get_scope(),
	case LeftExpr of
		{oo_remote, Ln2, {atom, _, self}, {var, _, AttrName}} ->
			IsObjMethod = (not st:is_static(Scope)) or st:is_constructor(Scope),
			case IsObjMethod of
				true ->
					ObjectID_AST = gen_ast:var(Ln2, "ObjectID"),
					AttrNameAST = gen_ast:atom(Ln2, AttrName),
					AttrValueAST = transform_inner_expr(RightExpr),
					UpdateArgs = [ObjectID_AST, AttrNameAST, AttrValueAST],
					rcall(Ln2, ooe, update_attr, UpdateArgs);

				false ->
					Error = gen_ast:atom(Ln2, {self_on_static_method}),
					{call, Ln1, gen_ast:atom(Ln2, throw), Error}
			end;

		_ ->
			Transformed_LeftExpr = transform_inner_expr(LeftExpr),
			Transformed_RightExpr = transform_inner_expr(RightExpr),
			match(Ln1, Transformed_LeftExpr, Transformed_RightExpr)
	end.

%%-----------------------------------------------------------------------------
%% percorre uma expressao transformando cada subelemento
%% falta definir todos os possíveis nós, no caso tuplas, listas e... oq mais?
%% para botar um fim na recursao

transform_inner_expr({oo_remote, Ln1, {atom, _, self}, ObjectAttr}) ->
	Scope = st:get_scope(),
	{_, _, AttrName} = ObjectAttr,
	IsObjMethod = (not st:is_static(Scope)) or st:is_constructor(Scope),
	case IsObjMethod of
		true ->
			ObjectID_AST = gen_ast:var(Ln1, "ObjectID"),
			LookupArgs = [ObjectID_AST, gen_ast:atom(Ln1, AttrName)],
			rcall(Ln1, ooe, lookup_attr, LookupArgs);

		false ->
			Error = gen_ast:atom(Ln1, {self_on_static_method}),
			{call, Ln1, gen_ast:atom(Ln1, throw), Error}
	end;

transform_inner_expr({oo_remote, Ln1, ObjectVar, ObjectAttr}) ->
	ObjectVar2 = call(Ln1, element, [integer(Ln1, 2), ObjectVar]),
	{_, _, AttrName} = ObjectAttr,
	LookupArgs = [ObjectVar2, gen_ast:atom(Ln1, AttrName)],
	rcall(Ln1, ooe, lookup_attr, LookupArgs);

%% chamadas de funcao
transform_inner_expr({call, _, _, _} = Expr) ->
	create_call(Expr);

%% operacoes aritimeticas, como +, -, ++, etc.
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

%% tuplas {E1, E2, ...}
transform_inner_expr({tuple, Ln, Elements}) ->
	{tuple, Ln, lists:map(fun transform_inner_expr/1, Elements)};

%% funs  fun() -> ... end
transform_inner_expr({'fun', Ln, {clauses, ClauseList}}) ->
	transform_fun(Ln, ClauseList);

transform_inner_expr(Expr) -> Expr.

%%-----------------------------------------------------------------------------
%% funcao para transformar chamada de funcoes
create_call({call, Ln1, FuncLocation, ArgList}) ->
	case get_func_loc(FuncLocation, ArgList) of
		{normal, TransfFuncLocation} ->
			TransfArgList = [transform_inner_expr(Arg) || Arg <- ArgList],
			{call, Ln1, TransfFuncLocation, TransfArgList};

		{object, ObjectVarName, FuncName} ->
			create_object_call(Ln1, ObjectVarName, FuncName, ArgList);

		{error, Error} ->
			{call, Ln1, gen_ast:atom(Ln1, throw), gen_ast:tuple(Error)}
	end.

%% chamadas de funcao Objecto::funcao(Args)
%% para metodo de objeto
create_object_call(Ln, ObjectVarName, FuncName, ArgList) ->
	TransfClassName = gen_ast:element(Ln, 1, var(Ln, ObjectVarName)),
	TransfObjectID  = gen_ast:element(Ln, 2, var(Ln, ObjectVarName)),

	TransfArgListTemp = [transform_inner_expr(Arg) || Arg <- ArgList],
	TransfArgList = [TransfObjectID | TransfArgListTemp],
	TransfFuncLocation = {remote, Ln, TransfClassName, FuncName},

	{call, Ln, TransfFuncLocation, TransfArgList}.

%%-----------------------------------------------------------------------------
%% busca a localizacao da funcao e verifica se eh static, public, etc
%% retorna:
%%   {normal, TransfLoc} - caso a traducao seja direta
%%   object              - caso seja metodo de objeto, que eh tratado diferente
%%
%% chamadas Objeto::funcao(Args)
get_func_loc({oo_remote, _Ln, {var, _, ObjectVarName}, FuncName}, _ArgList) ->
	{object, ObjectVarName, FuncName};

%% chamadas classe::funcao(Args)
get_func_loc({oo_remote, Ln2, {atom, _, ClassName}, FuncName}, ArgList) ->
	{_, _, FuncName2} = FuncName,

	MethodKey = {ClassName, {FuncName2, length(ArgList)}},

	TransfClassName = atom(Ln2, ClassName),

	case st:exist_class(ClassName) of
		false ->
			{error, {class_not_found, ClassName}};

		true ->
			case st:is_constructor(MethodKey) of
				true ->
					{normal, {remote, Ln2, TransfClassName, FuncName}};

				false ->
					case st:is_public(MethodKey) of
						true ->
							case st:is_static(MethodKey) of
								true ->
									{normal,
									  {remote, Ln2, TransfClassName, FuncName}};

								false ->
									{error, {not_static, ClassName, FuncName}}
							end;

						false ->
							{error, {not_public, ClassName, FuncName}}
					end
			end
	end;

%% chamadas de funcao modulo:funcao(Args)
get_func_loc({remote, Ln2, Module, Function}, _ArgList) ->
	{normal, {remote, Ln2, Module, Function}};

%% chamadas de funcao funcao(Args)
get_func_loc(FunctionName, _ArgList) ->
	{normal, FunctionName}.

%%-----------------------------------------------------------------------------
%% transforma expressoes fun() -> ... ; () -> ... end
transform_fun(Ln, ClauseList) ->
	TransfClauseList = lists:map(fun transform_fun_clause/1, ClauseList),
	{'fun', Ln, {clauses, TransfClauseList}}.

transform_fun_clause({clause, Ln, ArgList, [], ExprList}) ->
	TransfArgList = lists:map(fun match_param/1, ArgList),
	TransfExprList = lists:map(fun match_expr/1, ExprList),
	{clause, Ln, TransfArgList, [], TransfExprList}.
