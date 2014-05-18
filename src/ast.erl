%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Criacao e manipulacao da AST

-module(ast).
-export([get_urn_tokens/1, get_urn_forms/1, get_class_info/1, get_urn_forms_tokens/1]).
-include("../include/uarini_define.hrl").

%%-----------------------------------------------------------------------------
%% Extrai a Uarini Abstract Syntax Tree de um arquivo .cerl
get_urn_forms(FileName) ->
	Tokens = get_urn_tokens(FileName),
	lists:map(
		fun(Ts) ->
			case uarini_parse:parse(Ts) of
				{ok, Form} ->
					Form;
				{error, {Ln, uarini_parse, ErrorList}} ->
					io:format("*****temporary syntax error print*****\n"
								"'~p'#~p: ~p\n\n\n\n", [filename:basename(FileName), Ln, ErrorList]),
					error
					%% uarini_errors:handle_error(Ln, 15,
					%% 	[FileName, lists:flatten(ErrorList)]),
			end
		end,
		get_urn_forms_tokens(Tokens)).

%%-----------------------------------------------------------------------------
%% Extrai a lista de Tokens de um arquivo .cerl
get_urn_tokens(FileName) ->
    {ok, Tokens} = aleppo:process_file(FileName),
    [begin
        case element(2, T) of
          L when is_integer(L) -> T;
          {L, _C} -> setelement(2, T, L)
        end
     end || T <- Tokens, element(1, T) =/= eof].

%%-----------------------------------------------------------------------------
%% Quebra os forms de um fluxo de Tokens identificados por 'dot'
get_urn_forms_tokens(TokenStream) ->
	get_urn_forms_tokens(TokenStream, [], []).


get_urn_forms_tokens([], [], FormStack) ->
    lists:reverse(FormStack);

get_urn_forms_tokens([], TokenStack, FormStack) ->
    Form = lists:reverse(TokenStack),
    lists:reverse(
        update_form_stack(Form, FormStack));

get_urn_forms_tokens([DotToken={dot,_}|TokenStream], TokenStack, FormStack) ->
    Form = lists:reverse([DotToken|TokenStack]),
    get_urn_forms_tokens(TokenStream, [],
        update_form_stack(Form, FormStack));

get_urn_forms_tokens([Token|TokenStream], TokenStack, FormStack) ->
    get_urn_forms_tokens(TokenStream, [Token|TokenStack], FormStack).


update_form_stack(Form, []) -> [Form];
update_form_stack(Form, FormStack=[FormStackHead|FormStackTail]) ->
    case must_merge_form(
            get_form_first_token_id(FormStackHead),
            get_form_first_token_id(Form)) of
        true ->
            [FormStackHead++Form|FormStackTail];
        false ->
            [Form|FormStack]
    end.

must_merge_form(class_attributes, var) -> true;
must_merge_form(class_methods, atom) -> true;
must_merge_form(attributes, var) -> true;
must_merge_form(methods, atom) -> true;
must_merge_form(
    _FirstPrevFormTokenId,
    _FirstNextFormTokenId) -> false.

get_form_first_token_id([Token|_]) -> element(1, Token).

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
%%		{Nome, ValorInicial}
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
	MethodsInfo = ClassInfo#class.methods,
	MethodsInfo2 =
		[update_method(Mthd, ExportList) || Mthd <- MethodsInfo],
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
	case match_form(Form) of
		{class_name, ClassName} ->
			get_forms_info(FormList, ClassInfo#class{name = ClassName});

		{interface, InterfaceName} ->
			ClassInfo2 = ClassInfo#class{
							name = InterfaceName, is_interface = true},
			get_forms_info(FormList, ClassInfo2);

		{parent, ParentName} ->
			get_forms_info(FormList, ClassInfo#class{parent=ParentName});

		{implements, InterfaceList} when is_list(InterfaceList) ->
			get_forms_info(FormList, ClassInfo#class{impl=InterfaceList});

		{implements, InterfaceName} when is_atom(InterfaceName) ->
			get_forms_info(FormList, ClassInfo#class{impl=[InterfaceName]});

		{attributes, AttrList} ->
			get_forms_info(FormList, ClassInfo#class{attrs = AttrList});

		{methods, MethodList} ->
			OriginalMethodList = ClassInfo#class.methods,
			NewMethodList = OriginalMethodList ++ MethodList,
			get_forms_info(FormList, ClassInfo#class{methods = NewMethodList});

		{constructors, ConstrList} ->
			get_forms_info(FormList, ClassInfo#class{constr=ConstrList});

		{export, ExportList} ->
			get_forms_info(FormList, ClassInfo#class{export = ExportList});

		nop ->
			get_forms_info(FormList, ClassInfo)
	end.

%%-----------------------------------------------------------------------------
%% faz o match do form para extrair a informação correspondente

%% caso nao tenha atributos - DESATUALIZADO, MUDAR!!
%% match_form({instance_attributes, _}, [{class_methods, _} | _FormList]) ->
%% 	nop;

match_form({attribute, _, class, ClassName}) ->
	{class_name, ClassName};

match_form({attribute, _, interface, InterfaceName}) ->
	{interface, InterfaceName};

match_form({attribute, _, extends, ParentName}) ->
	{parent, ParentName};

match_form({attribute, _, implements, InterfaceName}) ->
	{implements, InterfaceName};

match_form({attribute, _, constructor, ConstrList}) ->
	{constructors, ConstrList};

match_form({attribute, _, export, ExportList}) ->
	{export, ExportList};

match_form({class_attributes, Ln, _AttrList}) ->
	uarini_errors:handle_error(Ln, 13, []),
	nop;

match_form({attributes, _Ln, AttrList}) ->
	AttrInfoList = get_attr_info(AttrList),
	{attributes, AttrInfoList};

match_form({class_methods, _Ln, MethodList}) ->
	ClassMethods = get_all_methods_info(static, MethodList),
	{methods, ClassMethods};

match_form({methods, _Ln, MethodList}) ->
	InstanceMethods = get_all_methods_info(object, MethodList),
	{methods, InstanceMethods};

match_form(_) -> nop.

%%-----------------------------------------------------------------------------
%% info de campos
get_attr_info(AttrList) ->
	get_attr_info(AttrList, []).

get_attr_info([], AttrInfoList) ->
	lists:reverse(AttrInfoList, []);
get_attr_info([Attr | Rest], AttrInfoList) ->
	{VarName, VarValue} =
		case Attr of
			{oo_attribute, _Ln, {var, _, Name}} ->
				{Name, {nil, 0}};

			{oo_attribute, _Ln, {var, _, Name}, InitialExpr} ->
				{Name, InitialExpr}
		end,

	VarKey = VarName,
	NewAttrInfo = {VarKey, VarValue},
	get_attr_info(Rest, [ NewAttrInfo | AttrInfoList ]).

%%-----------------------------------------------------------------------------
%% info dos metodos
%% Scope pode ser static ou object
get_all_methods_info(Scope, FunctionList) ->
	get_methods_info2(Scope, FunctionList, []).

get_methods_info2(_, [], MethodsInfo) ->
	lists:reverse(MethodsInfo);

get_methods_info2(Scope, [{function, _, Name, Arity, _} | Rest], MethodsInfo) ->
	MthdKey = {Name, Arity},

	MthdValue =
		case Scope of
			static -> [static];
			object -> []
		end,
	get_methods_info2(Scope, Rest, [{MthdKey, MthdValue} | MethodsInfo]).

%% atualiza informação dos métodos com modificador se for public ou nao 
update_method({MethodKey, MethodValue}, ExportList) ->
	IsPublic = helpers:has_element(MethodKey, ExportList),

	Modifiers =
		case IsPublic of
			true -> [public | MethodValue];
			false -> MethodValue
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
