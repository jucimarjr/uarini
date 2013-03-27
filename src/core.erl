%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Módulo central do compilador, traduz as expressoes em uarini  

-module(core).
-export([transform_uast_to_east/3]).
-include("../include/uarini_define.hrl").

-import(gen_ast, [match/3, var/2, rcall/4, atom/2, tuple/2]).

%%-----------------------------------------------------------------------------
%% Converte o uast em east.
%%   uast -> arvore sintatica do uarini.
%%   east -> arvore sintatica do erlang.
transform_uast_to_east(AST, ErlangModuleName, ClassesInfo) ->
	%% io:format("core: compilando \"~p\"...\n", [ErlangModuleName]),
	%% F = fun(Class, ClassInfo) ->
	%% 			io:format("\n-------~p--------\n~p\n------------\n", [Class, ClassInfo]) end,
	%% F2 = fun() -> G = get(), lists:map(fun({{oo_classes, Class}, Info}) -> F(Class, Info); (_) -> ok end,G) end,

	st:new(),
	st:put_scope(class, ErlangModuleName),
	st:insert_classes_info(ClassesInfo),

	ParentMethods = create_all_parent_methods(ErlangModuleName),
	ConstrList = st:get_all_constr_info(ErlangModuleName),

	st:merge_parent_members(ClassesInfo),
	uarini_errors:check_interface(ErlangModuleName),
	st:insert_default_constructors(ClassesInfo),

	DefaultConstructor = create_default_constructor(ErlangModuleName, ConstrList),
	OOFunctions = DefaultConstructor ++ ParentMethods,

	{FunctionList, OtherForms} = get_erl_forms(AST),
	FunctionList2 = OOFunctions ++ FunctionList,

	ExportList = st:get_export_list(ErlangModuleName),
	ExportList2 = filter_object_mthd(ExportList),

	ErlangModule =
		create_module(ErlangModuleName, FunctionList2, ExportList2, OtherForms),
%io:format("AFF: ~p\n\n", [ErlangModule]),

	case st:get_errors() of
		[] ->
			st:destroy(),
			{ok, ErlangModule};

		Errors ->
			st:destroy(),
			{error, Errors}
	end.

%%-----------------------------------------------------------------------------
%% filtra os metodos de objetos para acrescer a Arity, pois eles possuem
%% o ObjectID como parametro extra
filter_object_mthd(ExportList) ->
	filter_object_mthd(ExportList, []).

filter_object_mthd([], NewExportList) ->
	lists:reverse(NewExportList);

filter_object_mthd([FunctionInfo | ExportList], NewExportList ) ->
	{FunctionName, Arity} = FunctionInfo,
	ClassName = st:get_scope(class),
	MethodKey = {ClassName, {FunctionName, Arity}},

	case st:is_constructor(MethodKey) of
		true ->
			filter_object_mthd(ExportList, [FunctionInfo | NewExportList]);

		false ->
			case st:is_static(MethodKey) of
				true ->
				 filter_object_mthd(ExportList, [FunctionInfo | NewExportList]);

				false ->
				  NewFunctionInfo = {FunctionName, Arity + 1},
				  filter_object_mthd(ExportList, [NewFunctionInfo | NewExportList])
		end
	end.
			

%%-----------------------------------------------------------------------------
%% Filtra os forms, deixando apenas os especificos do erlang
%%    os forms do uarini sao tratados ao guardar as info das classes na ST
%%    eles representam os atributos e os metodos
%%    ExportList vem da info das classes, por isso eh ignorado
get_erl_forms(UariniForms) ->
	match_erl_form(UariniForms, [], []).

match_erl_form([], FunctionList, OtherForms) ->
	{lists:reverse(FunctionList), lists:reverse(OtherForms)};

match_erl_form([Form | UariniForms], FunctionList, OtherForms) ->
	case Form of
		{attribute, _Line, class, _ClassName} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{attribute, _Line, constructor, _Functions} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{attribute, _Line, extends, _Functions} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{attribute, _Line, export, _Functions} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{attribute, _Line, static, _Functions} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{class_attributes, _Line, _AttrList} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{attributes, _Line, _AttrList} ->
			match_erl_form(UariniForms, FunctionList, OtherForms);

		{class_methods, _Line, MthdsList} ->
			FunctionListTemp = lists:map(fun get_erl_function/1, MthdsList),
			N_FunctionList = FunctionList ++ FunctionListTemp,
			match_erl_form(UariniForms, N_FunctionList, OtherForms);

		{methods, _Line, MthdsList} ->
			FunctionListTemp = lists:map(fun get_erl_function/1, MthdsList),
			N_FunctionList = FunctionList ++ FunctionListTemp,
			match_erl_form(UariniForms, N_FunctionList, OtherForms);

		_AnyOtherForm ->
			N_OtherForms = [Form | OtherForms],
			match_erl_form(UariniForms, FunctionList, N_OtherForms)
	end.

%%-----------------------------------------------------------------------------
%% Percorre as clausulas dos metodos/funcoes e converte as expressoes de Uarini
%% para Erlang
get_erl_function({function, Line, Name, Arity, Clauses}) ->
	st:put_scope(function, {Name, Arity}),

	Transformed_Clauses = lists:map(fun get_erl_clause/1, Clauses),

	Scope = st:get_scope(),

	IsStaticOrConstructor = (st:is_static(Scope) or st:is_constructor(Scope)),
	case IsStaticOrConstructor of
		true ->
			{function, Line, Name, Arity, Transformed_Clauses};

		false ->
			{function, Line, Name, Arity + 1, Transformed_Clauses}
	end.

%%-----------------------------------------------------------------------------
%% Transforma uma clausula de uma funcao, convertendo as expressoes de Uarini
%% para Erlang
get_erl_clause({clause, Line, ParamList, [], ExprList}) ->
	TransfParamListTemp = lists:map(fun get_erl_param/1, ParamList),

	Scope = st:get_scope(),

	IsStaticOrConstructor = (st:is_static(Scope) or st:is_constructor(Scope)),
	case IsStaticOrConstructor of
		false ->
			TransfObjectID = gen_ast:var(Line, 'ObjectID'),
			TransfParamList = [TransfObjectID | TransfParamListTemp];

		true ->
			TransfParamList = TransfParamListTemp
	end,

	TransfExprList = lists:map(fun get_erl_expr/1, ExprList),

	case st:is_constructor(Scope) of
		false ->
			{clause, Line, TransfParamList, [], TransfExprList};

		true ->
			TransfExprList2=create_user_constr_body(Line, Scope, TransfExprList),
			{clause, Line, TransfParamList, [], TransfExprList2}
	end.

get_erl_param(Parameter) -> gen_erl_code:match_pattern(Parameter).
get_erl_expr(Expression) -> gen_erl_code:match_expr(Expression).

create_user_constr_body(Line, Scope, TransfExprList) ->
	{ScopeClass, _ScopeFunction} = Scope,
	AttrList = st:get_all_attr_info(ScopeClass),
	NewArgs = [
				tuple(Line, [atom(Line, AttrName), InitialExpr]) ||
				{AttrName, InitialExpr} <- AttrList],
	NewAST = rcall(Line, ooe, new, [gen_ast:list(Line, NewArgs)]),

	NewObjectID_AST = match(Line, var(Line, "ObjectID"), NewAST),
	ObjectID_AST = tuple(Line, [atom(Line,ScopeClass), var(Line, "ObjectID")]),

	[NewObjectID_AST] ++ TransfExprList ++ [ObjectID_AST].

%%-----------------------------------------------------------------------------
%% Cria o modulo a partir do east.
create_module(ErlangModuleName, FunctionList, ExportList, OtherForms) ->
	[ { attribute, 1, module, ErlangModuleName } |
		[{attribute, 2, export, ExportList}] ++ OtherForms ++
		FunctionList ++ [{eof, 1}]].

%%-----------------------------------------------------------------------------
%% declara o construtor padrao quando nao for definido nenhum pelo usuario
create_default_constructor(ClassName, []) ->
	Line = 0,

	AttrList = st:get_all_attr_info(ClassName),
	NewArgs = [
				tuple(Line, [atom(Line, AttrName), InitialExpr]) ||
				{AttrName, InitialExpr} <- AttrList],
	NewAST = rcall(Line, ooe, new, [gen_ast:list(Line,  NewArgs)]),

	NewObjectID_AST = match(Line, var(Line, "ObjectID"), NewAST),
	ObjectID_AST = gen_ast:tuple(Line,
						[gen_ast:atom(Line, ClassName), var(Line, "ObjectID")]),

	TransfExprList = [NewObjectID_AST, ObjectID_AST],
	Clause = {clause, Line, [], [], TransfExprList},

	[{function, Line, ?CONSTR_NAME, 0, [Clause]}];

%% caso jah tenha construtor def pelo user nao cria default_constructor
create_default_constructor(_ClassName, _ListaDeConstrutores) ->
	[].

%%-----------------------------------------------------------------------------
%% declara os métodos das super classes
%% isso soh funciona antes da funcao merge_parent_members ser chamada!
create_all_parent_methods(ClassName) ->
	[_ClassMethods | ParentClasses] = st:get_methods_with_parent(ClassName),
	create_parent_method_list(ParentClasses, []).

create_parent_method_list([], NewMethodList) -> NewMethodList;
create_parent_method_list([{ClassName, MethodList} | Rest], NewMethodList) ->
	MethodListTemp =
		[create_parent_method(Method, ClassName) || Method <- MethodList],
	create_parent_method_list(Rest, MethodListTemp ++ NewMethodList).

create_parent_method({{MethodName, Arity}, _Modifiers}, ClassName) ->
	Ln = 0,
	Arity2 =
		case st:is_static({ClassName, {MethodName, Arity}}) of
			false ->
				 Arity + 1;
			true ->
				Arity
		end,

	ParamList = create_generic_params(0, Arity2),

	ScopeClass = st:get_scope(class),
	ParentScopeClass = st:get_superclass(ScopeClass),

	ExprList = [gen_ast:rcall(Ln, ParentScopeClass, MethodName, ParamList)],
	Clause = {clause, Ln, ParamList, [], ExprList},
	{function, Ln, MethodName, Arity2, [Clause]}.

create_generic_params(Ln, Arity) ->
	Seq = lists:seq(1, Arity),
	[gen_ast:var(Ln, "Param_" ++ integer_to_list(Num)) || Num <- Seq].
