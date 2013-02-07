%% LUDUS - Laboratorio de Projetos Especiais em Engenharia da Computacao
%% Aluno  : Daniel Henrique ( dhbaquino@gmail.com )
%%			Emiliano Firmino ( elmiliox@gmail.com )
%%			Rodrigo Bernardino ( rbbernardino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Criacao e manipulacao das variaveis

-module(st).
-export(
	[
		%% criar / zerar dicionário
		new/0,		destroy/0,

		put_scope/2,	get_scope/0, get_scope/1,
		put_error/2,	get_errors/0,

		%% informações das classes
		insert_classes_info/1,	insert_parent_members/1,  exist_class/1,
		insert_default_constructor/2,
		is_constructor/1,		get_all_constr_info/1,
		is_static/1,			is_public/1,
		is_superclass/2,		get_superclass/1,
		exist_attr/2,			get_attr_info/2,		  get_all_attr_info/1,
		get_visible_methods/1,	get_export_list/1,
		get_methods_with_parent/1
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

put_error(Line, Code) ->
	NewErrors = [{Line, Code} | get(errors)],
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

%% inicializa "sub-dicionario" com informações das classes
insert_classes_info(ClassesInfoList) ->
	lists:map(fun put_class_info/1, ClassesInfoList).%,
	% insert_parent_members(ClassesInfoList).

%% insere informação de uma classe
put_class_info({{ClassName, ClassInfo}, Errors}) ->
	{ParentName, AttrList, ConstrList, ExportList, StaticList} = ClassInfo,
	put(errors, Errors),
	ClassKey = {oo_classes, ClassName},

	ClassValue = {ParentName, AttrList, ConstrList, ExportList, StaticList},
	put(ClassKey, ClassValue).

insert_default_constructor(ClassName, ConstrName) ->
	ClassInfo = get({oo_classes, ClassName}),
	{ParentName, AttrList, _ConstrList, ExportList, StaticList} = ClassInfo,
	ConstrList2 = [{ConstrName, 0}],
	ExportList2 = [{ConstrName, 0} | ExportList],	

	ClassKey = {oo_classes, ClassName},
	ClassValue = {ParentName, AttrList, ConstrList2, ExportList2, StaticList},
	put(ClassKey, ClassValue).
	

%% atualiza dicionário inserindo informações dos
%% métodos e atributos visíveis na superclasse
insert_parent_members([]) -> ok;
insert_parent_members([{{_, {null, _, _, _, _}, _}} | Rest]) ->
	insert_parent_members(Rest);
insert_parent_members([{{ClassName, ClassInfo}, _Errors} | Rest ]) ->
	{ParentName, AttrList, ConstrList, _ExportList, StaticList} = ClassInfo,

	MethodsWithParent = get_methods_with_parent(ClassName),
	NewStaticList = get_static_with_parent(StaticList, ClassName),

	NewExportList = merge_method_lists(MethodsWithParent),

	ParentAttrList  = get_visible_attr(ParentName, []),

	NewAttrList     = AttrList ++ ParentAttrList,

	ClassKey = {oo_classes, ClassName},
	ClassValue = {ParentName, NewAttrList, ConstrList, NewExportList, NewStaticList},
	put(ClassKey, ClassValue),
	insert_parent_members(Rest).

%% mescla os metodos de uma lista [{Classe1 Metodos1}, {Classe2, Metodos2} ...]
merge_method_lists(MethodsWithParent) ->
	MixedMethodList = [MethodList || {_, MethodList} <- MethodsWithParent],
	lists:flatten(MixedMethodList).

%% busca a informação de todos os campos visíveis às casses filhas
get_visible_attr(null, ODict)      -> ODict;
get_visible_attr(ClassName, ODict) ->
	{ParentName, AttrList, _, _, _} = get({oo_classes, ClassName}),
%%	VisibleAttr = lists:filter(fun is_visible_attr/1, AttrList),
%%	VisibleAttr ++
	NewODict = helpers:orddict_store_all(AttrList, ODict),
	get_visible_attr(ParentName, NewODict).

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
	AllMethods = get_visible_methods(ClassName),
	filter_over_methods(AllMethods).
	%% case filter_over_methods(AllMethods) of
	%% 	[ClassMethods_A | []] ->
	%% 		[ClassMethods_A, {null, []}];

	%% 	[ClassMethods_A, ClassMethods_B | _Rest] ->
	%% 		[ClassMethods_A, ClassMethods_B]
	%% end.

%% muito semelhante ao método acima, porem para buscar a lista de metodos static
%% que sao exportados!
%% alem disso, eh retornado apenas a lista de static, sem as classes
get_static_with_parent(StaticList, ClassName) ->
	SuperClassName = get_superclass(ClassName),
	ParentMethods = get_visible_methods(SuperClassName),
	filter_over_static([ {ClassName, StaticList} | ParentMethods ]).

%% busca todos os metodos public e static e retorna uma lista com eles
%% input (metodos public):   [{Classe1, Metodos1}, {Classe2, Metodos2} ... ]
%% output (public e static): [StaticMethod1, StaticMethods2, ...]
filter_over_static(MethodsList) ->
	lists:foldl(fun filter_over_static/2, [], MethodsList).

filter_over_static({Class, PublicList}, StaticList) ->
	StaticMethods = lists:filter(fun(X) -> is_static({Class, X}) end, PublicList),
	helpers:insert_replace_all(StaticMethods, StaticList).

%% busca a informação de todos os métodos visíveis às classes filhas
%% recursivamente indo de baixo para cima
%% recebe     - Classe1
%% retorna    - [{Classe1, Metodos1}, {Classe2, Metodos2}, ... ]
%% retorna no sentido crescente:
%% C --extende--|> B --extd--|> A    ->    {C, B, A}
%%
%% Esse formato eh necessario para gerar as funcoes proxy no modulo "core"
get_visible_methods(null)      -> [];
get_visible_methods(ClassName) ->
	{ParentName, _, _, ExportList, _} = get({oo_classes, ClassName}),
	[{ClassName, ExportList} | get_visible_methods(ParentName)].

%% dada as classes A <|-- B <|-- C (C extende B que extende A)
%% remove de A os métodos sobrescritos por B
%% recebe e retorna a lista CRESCENTE [C, B, A, ...]
%% o formato da lista recebida eh: [ {Classe1, Metodos1}, ... ]
filter_over_methods(MethodsList) ->
	ReverseMethodsList = lists:reverse(MethodsList, []),
	filter_over_methods(ReverseMethodsList, []).

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
	lists:filter(fun(MethodFrom_A) -> MethodFrom_A =/= MethodFrom_B end, MethodsList_A).

%%---------------------------------------------

%% verifica se classe existe
exist_class(ClassName) ->
	case get({oo_classes, ClassName}) of
		undefined  -> false;
		_ClassInfo -> true
	end.

%% verifica se a classe A é superclasse da classe B
is_superclass(Class_A, Class_B) ->
	{ParentName, _, _, _, _} = get({oo_classes, Class_B}),
	case ParentName of
		null    -> false;
		Class_A -> true;
		_Other  -> is_superclass(Class_A, ParentName)
	end.

%% retorna a superclasse da classe passada por parâmetro
get_superclass(ClassName) ->
	{ParentName, _, _, _, _} = get({oo_classes, ClassName}),
	ParentName.

%%----------------------------------------------------------------------------
%%                              CONSTRUTORES
%% verifica se determinado método é um construtor 
is_constructor({ClassName, {FunctionName, Arity}}) ->
	{_, _, ConstrList, _, _} = get({oo_classes, ClassName}),
	helpers:has_element({FunctionName, Arity}, ConstrList).

%% busca por todos os contrutores
get_all_constr_info(ClassName) ->
	{_, _, ConstrList, _, _} = get({oo_classes, ClassName}),
	ConstrList.

%%----------------------------------------------------------------------------
%%                              METODOS
%% verificoes de metodos 
is_static({ClassName, {FunctionName, Arity}}) ->
	{_, _, _, _, StaticList} = get({oo_classes, ClassName}),
	helpers:has_element({FunctionName, Arity}, StaticList).

is_public({ClassName, {FunctionName, Arity}}) ->
	{_, _, _, ExportList, _} = get({oo_classes, ClassName}),
	helpers:has_element({FunctionName, Arity}, ExportList).

get_export_list(ClassName) ->
	{_, _, _, ExportList, _} = get({oo_classes, ClassName}),
	ExportList.

%%----------------------------------------------------------------------------
%%                              CAMPOS
%%
%% busca informações de todos os campos declarados
get_all_attr_info(ClassName) ->
	{_, AttrList, _, _, _} = get({oo_classes, ClassName}),
	AttrList.

%% verifica se variável existe na classe
exist_attr(ClassName, AttrName) ->
	case get_attr_info(ClassName, AttrName) of
		false      -> false;
		_AttrInfo -> true
	end.

%% busca informações do campo de uma classe
get_attr_info(ClassName, AttrName) ->
	{_, AttrList, _, _, _} = get({oo_classes, ClassName}),
	case lists:keyfind(AttrName, 1, AttrList) of
		{AttrName, AttrValue} ->
			AttrValue;
		false ->
			false
	end.

%%                     FINAL INFO DAS CLASSES
%%----------------------------------------------------------------------------
