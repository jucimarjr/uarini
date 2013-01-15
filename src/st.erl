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

		put_scope/2,	get_scope/0,
		put_error/2,	get_errors/0,

		%% informações das classes
		insert_classes_info/1,	insert_parent_members/1,  exist_class/1,
		is_constructor/1,		get_all_constr_info/1,
		is_superclass/2,		get_superclass/1,
		exist_attr/2,			get_attr_info/2,		  get_all_attr_info/1
	]).

-import(helpers, [has_element/2]).

new() ->
	put(errors, []),
	put(scope, '__undefined__').

destroy() ->
	erase(), ok.

put_scope(class, Class) ->
	put({scope, class}, Class);

put_scope(function, Function) ->
	put({scope, function}, Function).

get_scope()    ->
	Class = get({scope, class}),
	Function = get({scope, function}),
	{Class, Function}.

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
	lists:map(fun put_class_info/1, ClassesInfoList),
	insert_parent_members(ClassesInfoList).

%% insere informação de uma classe
put_class_info({{ClassName, ClassInfo}, Errors}) ->
	{ParentName, AttrList, ConstrList, ExportList, StaticList} = ClassInfo,
	put(errors, Errors),
	ClassKey = {oo_classes, ClassName},
	ClassValue = {ParentName, AttrList, ConstrList, ExportList, StaticList},
	put(ClassKey, ClassValue).

%% atualiza dicionário inserindo informações dos
%% métodos e atributos visíveis na superclasse
insert_parent_members([]) -> ok;
insert_parent_members([{{_, null, _, _, _}, _} | Rest]) ->
	insert_parent_members(Rest);
insert_parent_members([ {{ClassName, ClassInfo}, _Errors} | Rest ]) ->
	{ParentName, AttrList, ConstrList, ExportList, StaticList} = ClassInfo,
	%ParentMethods = get_methods_with_parent(ClassName),
	ParentAttrList  = get_visible_attr(ParentName),

	%NewMethods = merge_parent_methods(ParentMethods),

	NewAttrList     = AttrList ++ ParentAttrList,

	ClassKey = {oo_classes, ClassName},
	ClassValue = {ParentName, NewAttrList, ConstrList, ExportList, StaticList},
	put(ClassKey, ClassValue),

	insert_parent_members(Rest).

%% busca a informação de todos os campos visíveis às casses filhas
get_visible_attr(null)      -> [];
get_visible_attr(ClassName) ->
	{ParentName, AttrList, _, _, _} = get({oo_classes, ClassName}),
	VisibleAttr = lists:filter(fun is_visible_attr/1, AttrList),
	VisibleAttr ++ get_visible_attr(ParentName).

is_visible_attr({_, {_, Modifiers}}) ->
	helpers:has_element(public, Modifiers).

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
