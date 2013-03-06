%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao3201;0c
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : traduz as expressoes em Uarini para Erlang

-module(gen_erl_code).
-export([match_param/1, match_expr/1]).
-include("../include/uarini_define.hrl").

-import(gen_ast,
	[
		match/3, rcall/4, integer/2, call/3, var/2, atom/2
	]).

-import(uarini_errors, [handle_error/3]).

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

%% operacoes aritimeticas, como +, -, ++, etc.
match_expr({op, _, _, _, _} = Expr) ->
	transform_inner_expr(Expr);

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
					ObjectID_AST = gen_ast:objectID(Ln2),
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

%% self::Atributo
transform_inner_expr({oo_remote, Ln1, {atom, _, self}, ObjectAttr}) ->
	Scope = st:get_scope(),
	{_, _, AttrName} = ObjectAttr,
	IsObjMethod = (not st:is_static(Scope)) or st:is_constructor(Scope),
	case IsObjMethod of
		true ->
			ObjectID_AST = gen_ast:objectID(Ln1),
			LookupArgs = [ObjectID_AST, gen_ast:atom(Ln1, AttrName)],
			rcall(Ln1, ooe, lookup_attr, LookupArgs);

		false ->
			handle_error(Ln1, 1, [])
	end;

%% Objeto::Atributo
transform_inner_expr({oo_remote, Ln1, {var, _,_} = ObjectVar, ObjectAttr}) ->
	ObjectVar2 = call(Ln1, element, [integer(Ln1, 2), ObjectVar]),
	{_, _, AttrName} = ObjectAttr,
	LookupArgs = [ObjectVar2, gen_ast:atom(Ln1, AttrName)],
	rcall(Ln1, ooe, lookup_attr, LookupArgs);

%% classe::Atributo  # ERRO
transform_inner_expr({oo_remote, Ln1, {atom, _, Name} = _WrongObjectVar, _}) ->
	uarini_errors:handle_error(Ln1, 10, [Name]);

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

		{object_direct, FuncName} ->
			create_object_direct_call(Ln1, FuncName, ArgList);

		{object_super, SuperClassName, FuncName} ->
			create_super_call(Ln1, SuperClassName, FuncName, ArgList);

		error ->
			error
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

%% chamadas de funcao self::funcao(Args)
%% ObjectID ja esta na propria funcao
create_object_direct_call(Ln, FuncName, ArgList) ->
	TransfObjectID  = gen_ast:objectID(Ln),

	TransfArgListTemp = [transform_inner_expr(Arg) || Arg <- ArgList],
	TransfArgList = [TransfObjectID | TransfArgListTemp],

	{call, Ln, FuncName, TransfArgList}.

%% chamadas de funcao super::funcao(Args)
%% ObjectID ja esta na propria funcao
create_super_call(Ln, SuperClassName, FuncName, ArgList) ->
	TransfObjectID  = gen_ast:objectID(Ln),

	TransfArgListTemp = [transform_inner_expr(Arg) || Arg <- ArgList],
	TransfArgList = [TransfObjectID | TransfArgListTemp],
	TransfFuncLocation = {remote, Ln, SuperClassName, FuncName},

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

%% chamadas super::funcao(Args)
get_func_loc({oo_remote, Ln1, {atom, _, super}, FuncName}, ArgList) ->
	{_, _, FuncName2} = FuncName,
	Scope = st:get_scope(),

	{ScopeClassName, _ScopeMethod} = Scope,
	SuperClassName = st:get_superclass(ScopeClassName),

	Check =
		case SuperClassName of
			null ->
				handle_error(Ln1, 2, []);

			_ ->
				MethodKey = {SuperClassName, {FuncName2, length(ArgList)}},
				case st:exist_method(MethodKey) of
					false ->
						handle_error(Ln1, 11, [FuncName2, SuperClassName]);

					true ->
						case st:is_static(MethodKey) of
							true ->
								handle_error(Ln1, 3, []);

							false ->
								case st:is_static(MethodKey) of
									true ->
										handle_error(Ln1, 4, []);

									false ->
										ok
								end
						end
				end
		end,

	case Check of
		ok ->
			SuperClassName2 = atom(Ln1, SuperClassName),
			{object_super, SuperClassName2, FuncName};
		error ->
			error
	end;

%% chamadas self::funcao(Args)
get_func_loc({oo_remote, Ln1, {atom, _, self}, FuncName}, ArgList) ->
	{_, _, FuncName2} = FuncName,
	Scope = st:get_scope(),

	{ClassName, _ScopeMethod} = Scope,

	case st:is_static(Scope) of
		true ->
			handle_error(Ln1, 5, []);

		false ->
			case st:is_static({ClassName, {FuncName2, length(ArgList)}}) of
				true ->
					{normal, FuncName};

				false ->
					{object_direct, FuncName}
			end
	end;

%% chamadas classe::funcao(Args)
get_func_loc({oo_remote, Ln1, {atom, _, ClassName}, FuncName}, ArgList) ->
	{_, _, FuncName2} = FuncName,

	MethodKey = {ClassName, {FuncName2, length(ArgList)}},

	TransfClassName = atom(Ln1, ClassName),

	case st:exist_class(ClassName) of
		false ->
			handle_error(Ln1, 6, [ClassName]);

		true ->
			case st:is_constructor(MethodKey) of
				true ->
					{normal, {remote, Ln1, TransfClassName, FuncName}};

				false ->
					case st:is_public(MethodKey) of
						true ->
							case st:is_static(MethodKey) of
								true ->
									{normal,
									  {remote, Ln1, TransfClassName, FuncName}};

								false ->
									{_,_, FuncName2} = FuncName,
									ErrorArgs =
										[ClassName, FuncName2, length(ArgList)],
									handle_error(Ln1, 7, ErrorArgs)
							end;

						false ->
							{_,_, FuncName2} = FuncName,
							ErrorArgs = [ClassName, FuncName2, length(ArgList)],
							handle_error(Ln1, 8, ErrorArgs)
					end
			end
	end;

%% chamadas de funcao modulo:funcao(Args)
get_func_loc({remote, Ln2, Module, Function}, _ArgList) ->
	{normal, {remote, Ln2, Module, Function}};

%% chamadas de funcao funcao(Args)
get_func_loc({atom, Ln, FunctionName}, ArgList) ->
	Scope = st:get_scope(),

	{ScopeClass, _ScopeMethod} = Scope,

	MethodKey = {ScopeClass, {FunctionName, length(ArgList)}},

	case st:exist_method(MethodKey) of
		false ->
			{normal, {atom, Ln, FunctionName}};
		true ->
			case st:is_static(MethodKey) of
				true ->
					{normal, {atom, Ln, FunctionName}};

				false ->
					case st:is_static(Scope) of
						true ->
							handle_error(5, Ln, []);

						false ->
							{object_direct, {atom, Ln, FunctionName}}
					end
			end
	end.

%%-----------------------------------------------------------------------------
%% transforma expressoes fun() -> ... ; () -> ... end
transform_fun(Ln, ClauseList) ->
	TransfClauseList = lists:map(fun transform_fun_clause/1, ClauseList),
	{'fun', Ln, {clauses, TransfClauseList}}.

transform_fun_clause({clause, Ln, ArgList, [], ExprList}) ->
	TransfArgList = lists:map(fun match_param/1, ArgList),
	TransfExprList = lists:map(fun match_expr/1, ExprList),
	{clause, Ln, TransfArgList, [], TransfExprList}.
