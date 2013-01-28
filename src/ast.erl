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

	D1 = orddict:new(),
	D2 = orddict:store(parent_name, null, D1),
	D3 = orddict:store(attributes, [], D2),
%	D4 = orddict:store(methods, [], D3),
	D5 = orddict:store(constructors, [], D3),
	D6 = orddict:store(export, [], D5),
	D7 = orddict:store(class_name, "", D6),
	D8 = orddict:store(static, [], D7),

	NewODict = get_forms_info(FormList, D8),

	{ok, ClassName} = orddict:find(class_name, NewODict),
	{ok, ParentName} = orddict:find(parent_name, NewODict),
	{ok, AttrList} = orddict:find(attributes, NewODict),
%	D4 = orddict:find(methods, [], D3),
	{ok, ConstrList} = orddict:find(constructors, NewODict),
	{ok, ExportList} = orddict:find(export, NewODict),
	{ok, StaticList} = orddict:find(static, NewODict),

	ExportList2 = ExportList ++ ConstrList,

	Errors = st:get_errors(),
	st:destroy(),

	ClassKey = ClassName,
	ClassValue = {ParentName, AttrList, ConstrList, ExportList2, StaticList},
	{{ClassKey, ClassValue}, Errors}.


%%-----------------------------------------------------------------------------
%% percorre a lista de forms
get_forms_info([], ODict) ->
	ODict;
get_forms_info([Form | FormList], ODict) ->
	case match_form(Form, FormList) of
		{class_name, ClassName, FormList2} ->
			NewODict = orddict:store(class_name, ClassName, ODict),
			get_forms_info(FormList2, NewODict);

		{parent, ParentName, FormList2} ->
			NewODict = orddict:store(parent_name, ParentName, ODict),
			get_forms_info(FormList2, NewODict);

		{attributes, AttrList, FormList2} ->
			NewODict = orddict:store(attributes, AttrList, ODict),
			get_forms_info(FormList2, NewODict);

		{constructors, ConstrInfo, FormList2} ->
			NewODict = orddict:store(constructors, ConstrInfo, ODict),
			get_forms_info(FormList2, NewODict);

		{export, ExportInfo, FormList2} ->
			NewODict = orddict:store(export, ExportInfo, ODict),
			get_forms_info(FormList2, NewODict);

		{static, StaticInfo, FormList2} ->
			NewODict = orddict:store(static, StaticInfo, ODict),
			get_forms_info(FormList2, NewODict);

		nop ->
			get_forms_info(FormList, ODict)
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

match_form({attribute, _, parent, ParentName}, FormList) ->
	{parent, ParentName, FormList};

match_form({attribute, _, constructor, ConstrList}, FormList) ->
	{constructors, ConstrList, FormList};

match_form({attribute, _, export, ExportList}, FormList) ->
	{export, ExportList, FormList};

match_form({attribute, _, static, StaticList}, FormList) ->
	{static, StaticList, FormList};

%% por enquanto, as info dos metodos necessarias estao nos atributos
%% "static", "constructors" e "exports"
match_form({class_methods, _}, _) -> nop;
match_form(_,_) -> nop.

%%-----------------------------------------------------------------------------
%% info de campos
get_attr_info({oo_attributes,_, AttrList}) ->
	get_attr_info(AttrList, []).

get_attr_info([], AttrInfoList) ->
	lists:reverse(AttrInfoList, []);
get_attr_info([Attr | Rest], AttrInfoList) ->
	{oo_attribute,_, ModifierList, {oo_var, _,  TypeTemp, NameTemp}} = Attr,
	{{atom, _, Type}, {var, _, Name}} = {TypeTemp, NameTemp},
	ModifierList2 = [Modifier || {Modifier, _} <- ModifierList],
	VarKey = Name,
	VarValue = {Type, ModifierList2},
	NewAttrInfo = {VarKey, VarValue},
	get_attr_info(Rest, [ NewAttrInfo | AttrInfoList ]).

%%-----------------------------------------------------------------------------








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
