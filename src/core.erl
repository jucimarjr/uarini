%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Módulo central do compilador, traduz as expressoes em uarini  

-module(core).
-export([transform_uast_to_east/3]).
-include("../include/uarini_define.hrl").

-import(gen_ast, [match/3, var/2, rcall/4]).

%%-----------------------------------------------------------------------------
%% Converte o uast em east.
%%   uast -> arvore sintatica do uarini.
%%   east -> arvore sintatica do erlang.
transform_uast_to_east(AST, ErlangModuleName, ClassesInfo) ->
	io:format("core: compilando \"~p\"...\n", [ErlangModuleName]),

	st:new(),
	st:insert_classes_info(ClassesInfo),

	%DefaultConstructor = create_default_constructor(ErlangModuleName),
	%ParentMethods      = create_all_parent_methods(ErlangModuleName),
	%OOFunctions = [DefaultConstructor] ++ ParentMethods,

	%% mescla infos dos membros das classes com os das suas superclasses
	%st:insert_parent_members(ClassesInfo),

	%% AST na realidade eh uma lista de forms
	st:put_scope(class, ErlangModuleName),
	{ExportList, FunctionList, OtherForms} = get_erl_forms(AST),

	%FunctionList2 = [OOFunctions | FunctionList],

	%% ExportList eh uma lista de funcoes assim: {Nome, Arity}
	%ExportList2 = add_parent_methods(ExportList),
	ConstrList = st:get_all_constr_info(ErlangModuleName),
	ExportList2 = ExportList ++ ConstrList,

	ErlangModule =
		create_module(ErlangModuleName, FunctionList, ExportList2, OtherForms),

	case st:get_errors() of
		[] ->
			st:destroy(),
			{ok, ErlangModule};
		Errors ->
			st:destroy(),
			throw({error, Errors})
	end.

%%-----------------------------------------------------------------------------
%% Filtra os forms, deixando apenas os especificos do erlang
%%    os forms do uarini sao tratados ao guardar as info das classes na ST
%%    eles representam os atributos e os metodos
get_erl_forms(UariniForms) ->
	match_erl_form(UariniForms, [], [], []).

match_erl_form([], ExportList, FunctionList, OtherForms) ->
	{ExportList, FunctionList, OtherForms};

match_erl_form([Form | UariniForms], ExportList, FunctionList, OtherForms) ->
	case Form of
		{attribute, _Line, class, _ClassName} ->
			match_erl_form(UariniForms, ExportList, FunctionList, OtherForms);

		{attribute, _Line, constructor, _Functions} ->
			match_erl_form(UariniForms, ExportList, FunctionList, OtherForms);

		{attribute, _Line, static, _Functions} ->
			match_erl_form(UariniForms, ExportList, FunctionList, OtherForms);

		{attribute, _Line, export, Functions} ->
			N_ExportList = ExportList ++ Functions,
			match_erl_form(UariniForms, N_ExportList, FunctionList, OtherForms);

		{class_attributes, _Line} ->
			[_Attributes | UariniForms_rest] = UariniForms,
			match_erl_form(UariniForms_rest,
							ExportList, FunctionList, OtherForms);

		{class_methods, _Line} ->
			match_erl_form(UariniForms, ExportList, FunctionList, OtherForms);

		{function, Line, Name, Arity, Clauses} ->
			st:put_scope(function, {Name, Arity}),
			Transformed_Method = get_erl_function(Line, Name, Arity, Clauses),
			N_FunctionList = [Transformed_Method | FunctionList],
			match_erl_form(UariniForms, ExportList, N_FunctionList, OtherForms)

		%% {constructor, ConstructorData} ->
		%% 	ConstructorAst = create_constructor(ClassName, ConstructorData),
		%% 	NewErlangModuleBody = [ConstructorAst | ErlangModuleBody],
		%% 	match_erl_member(ClassName, Rest, NewErlangModuleBody)
	end.

%%-----------------------------------------------------------------------------
%% Percorre as clausulas dos metodos/funcoes e converte as expressoes de Uarini
%% para Erlang
get_erl_function(Line, Name, Arity, Clauses) ->
	Transformed_Clauses = lists:map(fun get_erl_clause/1, Clauses),
	{function, Line, Name, Arity, Transformed_Clauses}.

%%-----------------------------------------------------------------------------
%% Transforma uma clausula de uma funcao, convertendo as expressoes de Uarini
%% para Erlang
get_erl_clause({clause, Line, ParamList, [], ExprList}) ->
	Transformed_ParamList = lists:map(fun get_erl_param/1, ParamList),

	Transformed_ExprList = lists:map(fun get_erl_expr/1, ExprList),

	Scope = st:get_scope(),
	{ScopeClass, _ScopeFunction} = Scope,

	AttrList = st:get_all_attr_info(ScopeClass),
	NewArgs = [gen_ast:atom(Line, AttrName) || {AttrName, _Value} <- AttrList],
	NewAST = rcall(Line, ooe, new, [gen_ast:list(Line,  NewArgs)]),

	NewObjectID_AST = match(Line, var(Line, "ObjectID"), NewAST),
	ObjectID_AST = gen_ast:tuple(Line,
						[gen_ast:atom(Line,ScopeClass), var(Line, "ObjectID")]),

	case st:is_constructor(Scope) of
		true ->
			Transformed_ExprList2 =
				[NewObjectID_AST | Transformed_ExprList] ++ [ObjectID_AST],
			{clause, Line, Transformed_ParamList, [], Transformed_ExprList2};

		false ->
			{clause, Line, Transformed_ParamList, [], Transformed_ExprList}
	end.

get_erl_param(Parameter) -> gen_erl_code:match_param(Parameter).
get_erl_expr(Expression) -> gen_erl_code:match_expr(Expression).


%%-----------------------------------------------------------------------------
%% Cria o modulo a partir do east.
create_module(ErlangModuleName, FunctionList, ExportList, OtherForms) ->
	[ { attribute, 1, module, ErlangModuleName } |
		[{attribute, 2, export, ExportList}] ++ OtherForms ++
		FunctionList ++ [{eof, 1}]].

%%-----------------------------------------------------------------------------
%% declara os métodos das super classes
%% create_all_parent_methods(ClassName) ->
%% 	AllMethodsList = st:get_methods_with_parent(ClassName),
%% 	[_ClassMethods | InheritedMethods] = AllMethodsList,
%% 	create_parent_method_list(InheritedMethods, []).

%% create_parent_method_list([], AllMethodsList) -> AllMethodsList;
%% create_parent_method_list([{ClassName, MethodsList} | Rest], Result) ->
%% 	TempL = [create_parent_method(Method, ClassName) || Method <- MethodsList],
%% 	create_parent_method_list(Rest, TempL ++ Result).

%% create_parent_method(MethodInfo, ClassName) ->

