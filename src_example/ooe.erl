%% Author: daaquino
%% Created: Nov 16, 2012
%% Description: TODO: Add description to ooe
-module(ooe).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([def_class/2,def_superClass/2, def_attribute/7, def_method/6]).

%%
%% API Functions
%%

def_class(Class, Type) ->
	put({class, Class}, {Class, Type}).

def_superClass(Class, SuperClass) ->
	put({superClass, SuperClass, Class}, SuperClass).
  
def_attribute(Attribute, Class, Type, Value, Diretiva, Static, Final) ->
	put({attribute, Attribute, get({class, Class})}, {Type, Value, Diretiva, Static, Final}).

def_method(Method, Class, Parameters, Diretiva, Abstract, Interface) ->
	put({method, Method, get({class, Class})}, {Parameters, Diretiva, Abstract, Interface}).

%%
%% Local Functions
%%