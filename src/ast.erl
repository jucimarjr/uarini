%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Criacao e manipulacao da AST

-module(ast).
-export([get_urn_tokens/1, get_urn_forms/1, get_class_info/1]).

%%-----------------------------------------------------------------------------
%% Extrai a Uarini Abstract Syntax Tree de um arquivo .cerl
get_urn_forms(FileName) ->
	Tokens = get_urn_tokens(FileName),
	lists:map(
		fun(Ts) ->
			{ok, Form} = uarini_parse:parse(Ts),
			Form
		end,
		split_dots(Tokens)).

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo .cerl
get_urn_tokens(FileName) ->
	{ok, FileContent} = file:read_file(FileName),
	Program = binary_to_list(FileContent),
	{ok, Tokens, _EndLine} = uarini_scan:string(Program),
	Tokens.

%%-----------------------------------------------------------------------------
%% Quebra os forms de um fluxo de Tokens identificados por 'dot'
split_dots(Ts) ->
	split_dots(Ts, [], []).

split_dots([], [], Fs) ->
    lists:reverse(Fs);
split_dots([], F, Fs) ->
    lists:reverse([lists:reverse(F)|Fs]);
split_dots([T={dot,_}|Ts], F, Fs) ->
    split_dots(Ts, [], [lists:reverse([T|F])|Fs]);
split_dots([T|Ts], F, Fs) ->
    split_dots(Ts, [T|F], Fs).

%%-----------------------------------------------------------------------------
%% Extrai informações da classe e seus  membros (campos e métodos)
%%
%% Estrutura do retorno:
%%		[Classe1, Classe2, ... ]
%%
%% ClasseN: {NomePai, Nome, Campos, Metodos, Construtores}
%%
%% Campos:  [Campo1, Campo2, ...]
%% Metodos: [Metodo1, Metodo2, ...]
%%
%% CampoN:
%%		{Nome, CampoValue}
%%			   |
%%			   |> {Tipo, Modificadores}
%%
%% MetodoN:
%%		{ MethodKey, MethodValue}
%%		  |			 |
%%		  |			 |> {TipoRetorno, Modificadores}
%%		  |
%%		  |> {Nome, Parametros}
%%
%% ConstrutorN:
%%		{ ConsrutorKey, ConstrutorValue }
%%		  |             |
%%		  |             |> Visibilidade
%%		  |> Parametros
%%
%% Outros:
%%		Tipo			=> atom()
%%		Nome			=> atom()
%%		Modificadores	=> [ atom() ]
%%		Parametros		=> [ Tipo ]
%%		Visibilidade    => atom()

%%-----------------------------------------------------------------------------
%% info das classes
get_class_info([_Form | _FormList]) ->
	st:new(),
	return_class_info(nil_ClassBody, nil_ClassName, nil_ParentName).

%%-----------------------------------------------------------------------------
%% retorna a info das classes com os erros encontrados
return_class_info(ClassBody, ClassName, ParentName) ->
	{FieldsInfo, MethodsInfo, ConstrInfo} =	get_members_info(ClassBody),
	FieldsInfo2    = lists:flatten(FieldsInfo),
	Errors = st:get_errors(),
	st:destroy(),
	{{ClassName, ParentName, FieldsInfo2, MethodsInfo, ConstrInfo}, Errors}.

%%-----------------------------------------------------------------------------
%% info de membros (método ou campo) e construtores
get_members_info(ClassBody) ->
	get_members_info(ClassBody, [], [], []).

get_members_info([], FieldsInfo, MethodsInfo, ConstructorsInfo) ->
	FieldsInfo2 = lists:reverse(FieldsInfo, []),
	MethodsInfo2 = lists:reverse(MethodsInfo, []),
	ConstructorsInfo2 = lists:reverse(ConstructorsInfo, []),
	{FieldsInfo2, MethodsInfo2, ConstructorsInfo2};

get_members_info([{method, MethodData} | Rest],
						FieldsInfo, MethodsInfo, ConstInfo) ->
	{_, ReturnJast, NameJast, ModifiersJast, ParameterList, _} = MethodData,
	{return, {_, Return}} = ReturnJast,
	{name, Name} = NameJast,
	{modifiers, ModifierList} = ModifiersJast,
	NewMethod = get_method_info(Name, ModifierList, Return, ParameterList),
	get_members_info(Rest, FieldsInfo, [NewMethod | MethodsInfo], ConstInfo);

%% TODO: Tratar Modificadores de campos
get_members_info([{var_declaration, VarTypeJast, VarList} | Rest],
						FieldsInfo, MethodsInfo, ConstInfo) ->
	{var_type, TypeJast}    = VarTypeJast,
	{var_list, VarJastList} = VarList,
	{_line, VarType} = TypeJast,
	NewField = get_fields_info(VarJastList, VarType),
	get_members_info(Rest, [NewField | FieldsInfo], MethodsInfo, ConstInfo);

%% checagem nome da classe x nome do construtor decladado realizada depois
get_members_info([{constructor, ConstData} | Rest],
						FieldsInfo, MethodsInfo, ConstInfo) ->
	{_, _Name, VisibilityJast, ParameterList, _} = ConstData,
	{visibility, Visibility} = VisibilityJast,
	NewConst = get_constructor_info(Visibility, ParameterList),
	get_members_info(Rest, FieldsInfo, MethodsInfo, [NewConst | ConstInfo]).

%%-----------------------------------------------------------------------------
%% info de métodos
get_method_info(MethodName, ModifierList, ReturnType, ParameterList) ->
	ParametersInfo = get_parameters_info(ParameterList),
	MethodKey      = {MethodName, ParametersInfo},
	MethodValue    = {ReturnType, ModifierList},
	{MethodKey, MethodValue}.

get_parameters_info(ParameterList) ->
	[ Type || {_, {_, {_, Type}}, {_, _}} <- ParameterList ].

%%-----------------------------------------------------------------------------
%% info de campos
get_fields_info(VarJastList, VarType) ->
	get_fields_info(VarJastList, VarType, []).

get_fields_info([], _, FieldsInfo) ->
	lists:reverse(FieldsInfo, []);
get_fields_info([VarJast | Rest], VarType, FieldsInfo) ->
	{{var, VarName}, _VarValue} = VarJast,
	VarKey = VarName,
	VarValue = {VarType, []}, %% TODO: [] = Modifiers
	NewFieldInfo = {VarKey, VarValue},
	get_fields_info(Rest, VarType, [ NewFieldInfo | FieldsInfo ]).

%%-----------------------------------------------------------------------------
%% construtores
get_constructor_info(Visibility, ParameterList) ->
	ParametersInfo = get_parameters_info(ParameterList),
	ConstructorKey = ParametersInfo,
	ConstructorValue    = Visibility,
	{ConstructorKey, ConstructorValue}.
