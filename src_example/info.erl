%% Author: daaquino
%% Created: Nov 16, 2012
%% Description: TODO: Add description to info
-module(info).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([load/0]).

%%
%% API Functions
%%

load() ->
	load_empregado(),
	load_horista(),
	load_mensalista(),
	load_polimorfismo().

%%
%% Local Functions
%

load_empregado() ->
	ooe:def_class(empregado, abstract),
	ooe:def_attribute(salario, empregado, null, 0, protected, false, false),
	ooe:def_method(setSalario, empregado, [null], public, false, false).

load_horista() ->
	ooe:def_class(horista, class),
	ooe:def_superClass(horista, class),
	ooe:def_method(getSalario, horista, [], public, false, false).

load_mensalista() ->
	ooe:def_class(mensalista, class),
	ooe:def_superClass(mensalista, empregado),
	ooe:def_method(getSalario, mensalista, [], public, false, false).

load_polimorfismo() ->
	ooe:def_class(polimorfismo, class),
	ooe:def_method(main, polimorfismo, [], public, false, false).