-module(core).
-export([transform_uast_to_east/2]).
-import(gen_ast,
	[
		function/4, var/2, atom/2, call/3, rcall/4, 'case'/3, clause/4,
		'fun'/2, string/2, tuple/2, atom/2
	]).
-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Converte o uast em east.
%%   uast -> arvore sintatica do cerl.
%%   east -> arvore sintatica do erlang.
transform_uast_to_east(CerlAST, ErlangModuleName) ->
	ErlangModuleBody =
		[get_erl_body(CerlClass)|| CerlClass <- CerlAST],
	ErlangModule = create_module(ErlangModuleName, ErlangModuleBody).

%%-----------------------------------------------------------------------------
%% Extrai o corpo do modulo erlang a partir de uma classe cerl
%% TODO: Tratar atributos ("variáveis globais") da classe...
get_erl_body(CerlClass) ->
	{_Line, _CerlClassName, {class_body, CerlClassBody}} = CerlClass,
	[get_erl_function(CerlMethod) || CerlMethod <- CerlClassBody].

%%-----------------------------------------------------------------------------
%% Extrai uma funcao erl de um metodo cerl
%% ISSUE: funciona apenas para métodos públicos
%% TODO: tratar visibilidade dos métodos quando trabalhar com POO.
get_erl_function({Line, _Type, {method, main}, Parameters,
					{block, _BlockLine, CerlMethodBody}})	 ->

	[{_Line, {var_type, {_Line, ArgClass}}, _ArgName}] = Parameters,

	case ArgClass of
		{array, 'String'} ->
			ok;
		_ ->
			uarini_exception:handle_error(Line, 4)
	end,
	st:put_scope(main),
	ErlangFunctionBody =
		get_erl_function_body(Line, CerlMethodBody, Parameters),
	function(Line, main, Parameters, ErlangFunctionBody);

get_erl_function({Line, {_TypeLine, TypeName},
			{method, FunctionIdentifier}, Parameters,
					{block, _BlockLine, CerlMethodBody}}) ->

	st:put_scope(FunctionIdentifier),
	put(type_method, TypeName),
	ErlangFunctionBody =
		get_erl_function_body(Line, CerlMethodBody, Parameters),
	function(Line, FunctionIdentifier, Parameters, ErlangFunctionBody).

%%-----------------------------------------------------------------------------
%% Converte o corpo do metodo cerl em funcao erlang.
%% TODO: Verificar melhor forma de detectar "{nil, Line}".
%% TODO: Declarar variável em qualquer lugar (mover o fun)
get_erl_function_body(Line, CerlMethodBody, ParametersList) ->
	ErlangArgsList =
		[var(ParamLine,"V_" ++ atom_to_list(ParamName)) ||
			{ParamLine, _ClassIdentifier, {parameter, ParamName}}
			<- ParametersList
		],

	MappedParamsFun =
		fun ({_VarLine, {var_type, {_Line, VarType}},
				{parameter, VarName}}) ->
			st:put_value({st:get_scope(), VarName}, {VarType, undefined})
		end,
	lists:map( MappedParamsFun, ParametersList),


	ScopeAst = atom(Line, st:get_scope()),
	InitArgs = [
		rcall(Line, st, put_value, [
			tuple(Line,	[ScopeAst, string(Line, InitArgName)]),
			tuple(Line,
				[gen_ast:type_to_ast(Line, InitArgType),
					var(Line, "V_" ++ atom_to_list(InitArgName))])]) ||
		({_Line, {var_type, {_Line, InitArgType}},
				{parameter, InitArgName}}) <-ParametersList
		],

	MappedErlangFun =
		fun(
			{var_declaration,
				{var_type,{VarLine, VarType}},
				{var_list, VarList}
			} = VarDeclaration
		) ->
			st:insert_var_list(VarLine, st:get_scope(), VarList, VarType),
			gen_erl_code:match_statement(VarDeclaration);

		(Statement) ->
			gen_erl_code:match_statement(Statement)
		end,

	New =
		case st:get_scope() of
			main ->
				[rcall(Line, st, new, [])];
			_ ->
				[]
		end,

	OldStack =
	case get(type_method) of
		void ->
			[rcall(Line, st, get_old_stack,
				[atom(Line, st:get_scope())])];
		_ ->
			[]
	end,

	Destroy = case st:get_scope() of
		main ->
			[rcall(Line,st, destroy, [])];
		_ ->
			[]
	end,


	ErlangStmtTemp1 =
		New ++
		[rcall(Line, st, get_new_stack,[atom(Line, st:get_scope())])]
		++
		InitArgs ++
		lists:map(MappedErlangFun, CerlMethodBody) ++
		OldStack ++ Destroy,

	ErlangStmt = [
			Element ||
			Element <- ErlangStmtTemp1,
			Element =/= no_operation
			],
	[{clause, Line, ErlangArgsList, [], ErlangStmt}].

%%-----------------------------------------------------------------------------
%% Cria o modulo a partir do east.
create_module(Name, ErlangAST) ->
	[ { attribute, 1, module, Name },{ attribute, 2, compile, export_all },
			{attribute, 3, import, {loop, [{for, 3}, {while, 2}]}},
			{attribute ,6, import, {vector,[{new,1},{get_vector,1}]}},
			{attribute ,7, import, {matrix,[{new_matrix,1},
						{creation_matrix,2}]}},
			{attribute, 4, import, {randomLib, [{function_random, 2}]}}]
	++ hd(ErlangAST) ++ [ { eof, 1 }].
