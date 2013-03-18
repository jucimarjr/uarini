%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Criacao e manipulacao das variaveis

-module(st).
-include("../include/uarini_define.hrl").
-export(
	[
		%% criar / zerar dicionário
		new/0,		destroy/0,

		put_scope/2,	get_scope/0, get_scope/1,
		put_error/3,	get_errors/0,

		%% informações das classes
		insert_classes_info/1,	merge_parent_members/1,  exist_class/1,
		insert_default_constructors/1,
		is_constructor/1,		get_all_constr_info/1,
		is_static/1,			is_public/1,
		is_superclass/2,		get_superclass/1,
		exist_attr/2,			get_attr_info/2,		get_all_attr_info/1,
		get_all_methods/1,		get_export_list/1,		exist_method/1,
		get_methods_with_parent/1,		get_methods_with_parent_2/1,
		get_interface_list/1
	]).

-import(helpers, [has_element/2]).

new() ->
	put(errors, []),
	put(scope, '__undefined__').

destroy() ->
	erase(), ok.

put_scope(class, Class) ->
	put({scope, class}, Class);

%% Function = {NomeFuncao, Arity}
put_scope(function, Function) ->
	put({scope, function}, Function).

get_scope()    ->
	Class = get({scope, class}),
	Function = get({scope, function}),
	{Class, Function}.

get_scope(class) ->
	get({scope, class}).

put_error(Line, Code, Args) ->
	NewErrors = [{Line, Code, Args} | get(errors)],
	put(errors, NewErrors).

get_errors() ->
	lists:reverse(get(errors), []).

%%----------------------------------------------------------------------------
%%                            INFO DAS CLASSES
%% Dicionario de classes
%% Estrutura do dicionario:
%% Chave:
%%		{oo_classes, NomeDaClasse}
%% Onde:
%%		NomeDaClasse => atom()
%%
%% Valor:
%%		{NomeSuper, Atributos, ListaConstrutores, ListaExport, ListaStatic}
%% Campos:  [Campo1, Campo2, ...]
%%
%% CampoN:
%%		{Nome, ValorInicial}
%%
%% ConstrutorN, ExportN:
%%		{ nome_funcao, QtdParametros }
%%
%% Outros:
%%		Tipo			=> atom()
%%		Nome			=> atom()
%%		Modificadores	=> [ atom() ]

%% inicializa "sub-dicionario" com informações das classes
insert_classes_info(ClassesInfoList) ->
	lists:map(fun put_class_info/1, ClassesInfoList).

%% insere informação de uma classe
put_class_info({{ClassName, ClassInfo}, Errors}) ->
	put(errors, Errors),

	ClassKey = {oo_classes, ClassName},
	ClassValue = ClassInfo,
	put(ClassKey, ClassValue).

%% atualiza dicionário inserindo informações dos construtores padrão
insert_default_constructors([]) -> ok;
insert_default_constructors([{{ClassName, _ClassInfo}, _Errors} | Rest]) ->
	ClassInfo = get({oo_classes, ClassName}),
	#class{constr = ConstrList} = ClassInfo,

	case ConstrList of
		[] ->
			ConstrList2 = [{?CONSTR_NAME, 0}];

		_ ->
			ConstrList2 = ConstrList
	end,

	ClassKey = {oo_classes, ClassName},
	ClassValue = ClassInfo#class{constr=ConstrList2},
	put(ClassKey, ClassValue),
	insert_default_constructors(Rest).

%% atualiza dicionário inserindo informações dos
%% métodos e atributos visíveis na superclasse
merge_parent_members([]) -> ok;
merge_parent_members([{{_, #class{parent = null}}, _} | Rest]) ->
	merge_parent_members(Rest);
merge_parent_members([{{ClassName, ClassInfo}, _Errors} | Rest ]) ->
	#class{parent = ParentName, attrs = AttrList} = ClassInfo,

	MethodsWithParent = get_methods_with_parent(ClassName),

	NewMethodsTemp = merge_method_lists(MethodsWithParent),
	NewMethods = orddict:from_list(NewMethodsTemp),

	ParentAttrList  = get_all_attr(ParentName, []),

	NewAttrList     = AttrList ++ ParentAttrList,

	ClassKey = {oo_classes, ClassName},
	ClassValue = ClassInfo#class{attrs = NewAttrList, methods = NewMethods},
	put(ClassKey, ClassValue),
	merge_parent_members(Rest).

%% mescla os metodos de uma lista [{Classe1 Metodos1}, {Classe2, Metodos2} ...]
merge_method_lists(MethodsWithParent) ->
	MixedMethodList = [MethodList || {_, MethodList} <- MethodsWithParent],
	lists:flatten(MixedMethodList).

%% busca a informação de todos os campos visíveis às casses filhas
get_all_attr(null, ODict)      -> ODict;
get_all_attr(ClassName, ODict) ->
	#class{parent=ParentName, attrs=AttrList} = get({oo_classes, ClassName}),
	NewODict = helpers:orddict_store_all(AttrList, ODict),
	get_all_attr(ParentName, NewODict).

%%is_visible_attr({_, {_, Modifiers}}) ->
%%	helpers:has_element(public, Modifiers).

%% busca as infos de todos os métodos visíveis de determinada classe,
%% acrescentando os métodos herdados e aplicando a sobrescrita (filtra métodos
%% sobrescritos da classe filha)
%%
%% retorna no formato [ {Classe, Metodos}, {SuperClasse, MetodosSuper} ]
%% métodos sobrescritos são RETIRADOS das classes de origem
%%
%% se D --extende--|> C --extende--|> B --extende--|> A, entao os metodos
%% A lista de B contem os metodos public de C e D
get_methods_with_parent(ClassName) ->
	AllMethods = get_all_methods(ClassName),
	filter_over_methods(AllMethods).

%% mesmo que o de cima acrescentando a mesclagem para retornar os métodos no
%% formato [metodo1, metodo2, ...]
get_methods_with_parent_2(ClassName) ->
	MethodsWithParent = get_methods_with_parent(ClassName),
	merge_method_lists(MethodsWithParent).

%% busca a informação de todos os métodos visíveis às classes filhas
%% recursivamente indo de baixo para cima
%% recebe     - Classe1
%% retorna    - [{Classe1, Metodos1}, {Classe2, Metodos2}, ... ]
%% retorna no sentido crescente:
%% C --extende--|> B --extd--|> A    ->    {C, B, A}
%%
%% Esse formato eh necessario para gerar as funcoes proxy no modulo "core"
get_all_methods(null)      -> [];
get_all_methods(ClassName) ->
	ClassInfo = get({oo_classes, ClassName}),
	#class{parent = ParentName, methods = MethodList} = ClassInfo,
	[{ClassName, MethodList} | get_all_methods(ParentName)].

%% dada as classes A <|-- B <|-- C (C extende B que extende A)
%% remove de A os métodos sobrescritos por B
%% recebe e retorna a lista CRESCENTE [C, B, A, ...]
%% o formato da lista recebida eh: [ {Classe1, Metodos1}, ... ]
filter_over_methods(MethodsList) ->
	filter_over_methods(lists:reverse(MethodsList), []).

filter_over_methods([], NewMethodsList) ->
	NewMethodsList;
filter_over_methods([LastMethods], NewMethodsList) ->
	[LastMethods | NewMethodsList];
filter_over_methods([MethodsA, MethodsB | Rest], NewMethodsList) ->
	{ClassA, MethodsListA} = MethodsA,
	{_ClassB, MethodsListB} = MethodsB,

	NewMethodsListA = remove_same_methods(MethodsListA, MethodsListB),

	NewMethodsA = {ClassA, NewMethodsListA},
	filter_over_methods([MethodsB | Rest], [NewMethodsA | NewMethodsList]).

%% considere B --extende--|> A
%% remove todos os métodos da lista A que estão em B e são iguais
%% usado fazer a sobrescrita, retorna a lista de métodos que B consegue ver
%% de A (sua classe pai)
remove_same_methods(MethodsListA, MethodsListB) ->
	lists:foldl(fun remove_method/2, MethodsListA, MethodsListB).

remove_method(MethodFrom_B, MethodsList_A) ->
	{Key_B, _} = MethodFrom_B,
	lists:filter(fun({Key_A, _}) -> Key_A =/= Key_B end, MethodsList_A).

%%---------------------------------------------

%% verifica se classe existe
exist_class(ClassName) ->
	case get({oo_classes, ClassName}) of
		undefined  -> false;
		_ClassInfo -> true
	end.

%% verifica se a classe A é superclasse da classe B
is_superclass(Class_A, Class_B) ->
	#class{parent = ParentName} = get({oo_classes, Class_B}),
	case ParentName of
		null    -> false;
		Class_A -> true;
		_Other  -> is_superclass(Class_A, ParentName)
	end.

%% retorna a superclasse da classe passada por parâmetro
get_superclass(ClassName) ->
	#class{parent = ParentName} = get({oo_classes, ClassName}),
	ParentName.

%%----------------------------------------------------------------------------
%%                              CONSTRUTORES
%% verifica se determinado método é um construtor 
is_constructor({ClassName, {FunctionName, Arity}}) ->
	#class{constr = ConstrList} = get({oo_classes, ClassName}),
	has_element({FunctionName, Arity}, ConstrList).

%% busca por todos os contrutores
get_all_constr_info(ClassName) ->
	#class{constr = ConstrList} = get({oo_classes, ClassName}),
	ConstrList.

%%----------------------------------------------------------------------------
%%                              METODOS
%% verificoes de metodos 
is_static({ClassName, MethodKey}) -> has_modifier(static, {ClassName, MethodKey}).
is_public({ClassName, MethodKey}) -> has_modifier(public, {ClassName, MethodKey}).

has_modifier(Modif, {ClassName, MethodKey}) ->
	#class{methods = MethodList} = get({oo_classes, ClassName}),
	{ok, Modifiers} = orddict:find(MethodKey, MethodList),
	has_element(Modif, Modifiers).

get_export_list(ClassName) ->
% pega info dos metodos e construtores e cria a export_list
	#class{methods=MethodList, constr=ConstrList} = get({oo_classes, ClassName}),
%	PublicMethods = lists:filter(
%				fun({_, Modf}) -> has_element(public, Modf) end, MethodList),
%	ExportListTemp = [MethodKey || {MethodKey,_} <- PublicMethods],
	ExportListTemp = [MethodKey || {MethodKey,_} <- MethodList],
	ExportListTemp ++ ConstrList.

exist_method({ClassName, MethodKey}) ->
	#class{methods = MethodList} = get({oo_classes, ClassName}),
	case orddict:find(MethodKey, MethodList) of
		{ok, _} -> true;
		_       -> false
	end.


%%----------------------------------------------------------------------------
%%                              CAMPOS
%%
%% busca informações de todos os campos declarados
get_all_attr_info(ClassName) ->
	#class{attrs = AttrList} = get({oo_classes, ClassName}),
	AttrList.

%% verifica se variável existe na classe
exist_attr(ClassName, AttrName) ->
	case get_attr_info(ClassName, AttrName) of
		false      -> false;
		_AttrInfo -> true
	end.

%% busca informações do campo de uma classe
get_attr_info(ClassName, AttrName) ->
	#class{attrs = AttrList} = get({oo_classes, ClassName}),
	case lists:keyfind(AttrName, 1, AttrList) of
		{AttrName, AttrValue} ->
			AttrValue;
		false ->
			false
	end.

%%----------------------------------------------------------------------------
%%                              INTERFACE

%% verifica se a classe implementa alguma interface, se sim, retorna o nome
get_interface_list(ClassName) ->
	ClassInfo = get({oo_classes, ClassName}),
	#class{impl = InterfaceNameList} = ClassInfo,
	InterfaceNameList.

%%                     FINAL INFO DAS CLASSES
%%----------------------------------------------------------------------------
