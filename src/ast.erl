%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Criacao e manipulacao da AST

-module(ast).
-export([get_urn_tokens/1, get_urn_forms/1, get_class_info/1]).
-include("../include/uarini_define.hrl").

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
    {ok, Tokens} = aleppo:scan_file(FileName),
    [remove_column(T) || T <- Tokens, element(1, T) =/= eof].

remove_column(TupleToken) ->
    [Lexema|[{L,_C}|Etc]] = tuple_to_list(TupleToken),
    list_to_tuple([Lexema|[L|Etc]]).

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
%% 		{{ClassName, ClassValue}, Errors}
%%      ClassValue =
%%			{NomeSuper, Atributos, ListaConstrutores, ListaExport, ListaStatic}
%%
%% Campos:  [Campo1, Campo2, ...]
%%
%% CampoN:
%%		{Nome, CampoValue}
%%			   |
%%			   |> {Tipo, Modificadores}
%%
%% ConstrutorN, ExportN, StaticN:
%%		{ nome_funcao, QtdParametros }
%%
%% Outros:
%%		Tipo			=> atom()
%%		Nome			=> atom()
%%		Modificadores	=> [ atom() ]

%%-----------------------------------------------------------------------------
%% busca por informacoes das classes armazenando o que encontrar num orddict
get_class_info(FormList) ->
	st:new(),

	ClassInfo = get_forms_info(FormList, #class{}),

	ExportList = ClassInfo#class.export,
	StaticList = ClassInfo#class.static,
	MethodsInfo = ClassInfo#class.methods,
	MethodsInfo2 =
		[update_method(Mthd, ExportList, StaticList) || Mthd <- MethodsInfo],
	MethodsInfo3 = orddict:from_list(MethodsInfo2),

	ExportList2 = ClassInfo#class.export ++ ClassInfo#class.constr,

	AttrList = orddict:from_list(ClassInfo#class.attrs),

	Errors = st:get_errors(),
	st:destroy(),

	ClassKey = ClassInfo#class.name,
	ClassValue = ClassInfo#class{export = ExportList2, methods = MethodsInfo3,
								attrs = AttrList},

	{{ClassKey, ClassValue}, Errors}.


%%-----------------------------------------------------------------------------
%% percorre a lista de forms
get_forms_info([], ClassInfo) ->
	ClassInfo;
get_forms_info([Form | FormList], ClassInfo) ->
	case match_form(Form, FormList) of
		{class_name, ClassName, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{name = ClassName});

		{parent, ParentName, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{parent=ParentName});

		{attributes, AttrList, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{attrs = AttrList});

		{methods, MethodList, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{methods = MethodList});

		{constructors, ConstrList, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{constr=ConstrList});

		{export, ExportList, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{export = ExportList});

		{static, StaticList, FormList2} ->
			get_forms_info(FormList2, ClassInfo#class{static = StaticList});

		nop ->
			get_forms_info(FormList, ClassInfo)
	end.

%%-----------------------------------------------------------------------------
%% faz o match do form para extrair a informação correspondente

%% caso nao tenha atributos
match_form({class_attributes, _}, [{class_methods, _} | _FormList]) ->
	nop;

match_form({class_attributes, _}, [AttrList | FormList]) ->
	AttrInfoList = get_attr_info(AttrList),
	{attributes, AttrInfoList, FormList};

match_form({attribute, _, class, ClassName}, FormList) ->
	{class_name, ClassName, FormList};

match_form({attribute, _, extends, ParentName}, FormList) ->
	{parent, ParentName, FormList};

match_form({attribute, _, constructor, ConstrList}, FormList) ->
	{constructors, ConstrList, FormList};

match_form({attribute, _, export, ExportList}, FormList) ->
	{export, ExportList, FormList};

match_form({attribute, _, static, StaticList}, FormList) ->
	{static, StaticList, FormList};

match_form({class_methods, _}, FormList) ->
	{MethodList, RestForms} = get_all_method_info(FormList),
	{methods, MethodList, RestForms};

match_form(_,_) -> nop.

%%-----------------------------------------------------------------------------
%% info de campos
get_attr_info({oo_attributes, _, AttrList}) ->
	get_attr_info(AttrList, []).

get_attr_info([], AttrInfoList) ->
	lists:reverse(AttrInfoList, []);
get_attr_info([Attr | Rest], AttrInfoList) ->
	{oo_attribute, _, TypeTemp, NameTemp} = Attr,
	{atom, _, Type} = TypeTemp,

	{VarName, VarValue} =
		case NameTemp of
			{var, _, Name} ->
				{Name, {Type, {nil, 0}}};

			{{var,_, Name}, {initial_value, InitialExpr}} ->
				{Name, {Type, InitialExpr}}
		end,

	VarKey = VarName,
	NewAttrInfo = {VarKey, VarValue},
	get_attr_info(Rest, [ NewAttrInfo | AttrInfoList ]).

%%-----------------------------------------------------------------------------
%% info dos metodos
get_all_method_info(FunctionList) ->
	get_method_info(FunctionList, []).

get_method_info([{function, _Ln, Name, Arity, _Clauses} | Rest], MethodsInfo) ->
	MthdKey = {Name, Arity},
	MthdValue = [],

	get_method_info(Rest, [{MthdKey, MthdValue} | MethodsInfo]);

get_method_info([], MethodsInfo) ->
	{lists:reverse(MethodsInfo), []};
get_method_info([NotAFunction | Rest], MethodsInfo) ->
	{lists:reverse(MethodsInfo), [NotAFunction | Rest]}.

%% atualiza informação dos métodos com seus respectivos modificadores
update_method({MethodKey, _MethodValue}, ExportList, StaticList) ->
	IsPublic = helpers:has_element(MethodKey, ExportList),
	IsStatic = helpers:has_element(MethodKey, StaticList),

	Modifiers =
		case {IsPublic, IsStatic} of
			{true, true} -> [public, static];
			{true, false} -> [public];
			{false, true} -> [static];
			{false, false} -> []
		end,

	{MethodKey, Modifiers}.





%% match_form({method, MethodData}) ->
%% 	{_, ReturnJast, NameJast, ModifiersJast, ParameterList, _} = MethodData,
%% 	{return, {_, Return}} = ReturnJast,
%% 	{name, Name} = NameJast,
%% 	{modifiers, ModifierList} = ModifiersJast,
%% 	NewMethod = get_method_info(Name, ModifierList, Return, ParameterList),
%% 	get_members_info(Rest, FieldsInfo, [NewMethod | MethodsInfo], ConstInfo);

%% %% TODO: Tratar Modificadores de campos
%% get_members_info([{var_declaration, VarTypeJast, VarList} | Rest],
%% 						FieldsInfo, MethodsInfo, ConstInfo) ->
%% 	{var_type, TypeJast}    = VarTypeJast,
%% 	{var_list, VarJastList} = VarList,
%% 	{_line, VarType} = TypeJast,
%% 	NewField = get_fields_info(VarJastList, VarType),
%% 	get_members_info(Rest, [NewField | FieldsInfo], MethodsInfo, ConstInfo);

%% %% checagem nome da classe x nome do construtor decladado realizada depois
%% get_members_info([{constructor, ConstData} | Rest],
%% 						FieldsInfo, MethodsInfo, ConstInfo) ->
%% 	{_, _Name, VisibilityJast, ParameterList, _} = ConstData,
%% 	{visibility, Visibility} = VisibilityJast,
%% 	NewConst = get_constructor_info(Visibility, ParameterList),
%% 	get_members_info(Rest, FieldsInfo, MethodsInfo, [NewConst | ConstInfo]).

%% %%-----------------------------------------------------------------------------
%% %% info de métodos
%% get_method_info(MethodName, ModifierList, ReturnType, ParameterList) ->
%% 	ParametersInfo = get_parameters_info(ParameterList),
%% 	MethodKey      = {MethodName, ParametersInfo},
%% 	MethodValue    = {ReturnType, ModifierList},
%% 	{MethodKey, MethodValue}.

%% get_parameters_info(ParameterList) ->
%% 	[ Type || {_, {_, {_, Type}}, {_, _}} <- ParameterList ].

%% %%-----------------------------------------------------------------------------
%% %% construtores
%% get_constructor_info(Visibility, ParameterList) ->
%% 	ParametersInfo = get_parameters_info(ParameterList),
%% 	ConstructorKey = ParametersInfo,
%% 	ConstructorValue    = Visibility,
%% 	{ConstructorKey, ConstructorValue}.
