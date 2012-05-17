Nonterminals

start_parser 
class_definition oo_definitions
extends_definition export_definition constructor_definition
class_attributes  attributes_definition attributes_definition_list
export_list attributes_list value
class_methods.


Terminals

'=' '::' '(' ')' '[' ']' '{' '}' ',' ';' '.' '/' '->'  
integer float identifier text 
class export extends constructor attributes
'class_attributes.' 'class_methods.'.


Rootsymbol start_parser.


start_parser -> class_definition oo_definitions class_methods
		: ['$1', '$2', '$3'].
start_parser -> class_definition oo_definitions class_attributes class_methods
		: ['$1', '$2', '$3', '$4'].
start_parser -> class_definition oo_definitions : ['$1' | '$2'].

oo_definitions -> export_definition 				: ['$1'].
oo_definitions -> export_definition constructor_definition 	: ['$1', '$2'].
oo_definitions -> constructor_definition export_definition  	: ['$1', '$2'].
oo_definitions -> extends_definition constructor_definition 	: ['$1', '$2'].
oo_definitions -> constructor_definition extends_definition  	: ['$1', '$2'].
oo_definitions -> export_definition constructor_definition extends_definition
		: ['$1', '$2', '$3'].
oo_definitions -> export_definition extends_definition constructor_definition
		: ['$1', '$2', '$3'].
oo_definitions -> constructor_definition export_definition extends_definition
		: ['$1', '$2', '$3'].
oo_definitions -> constructor_definition  extends_definition export_definition
		: ['$1', '$2', '$3'].
oo_definitions -> extends_definition export_definition constructor_definition
		: ['$1', '$2', '$3'].
oo_definitions -> extends_definition constructor_definition export_definition 
		: ['$1', '$2', '$3'].

class_definition -> class '(' identifier ')' '.'
		: {class, unwrap('$3')}.

constructor_definition -> constructor '(' identifier '/' integer ')' '.'
		: {constructor, unwrap('$3'), unwrap('$5')}.

extends_definition -> extends '(' identifier ')' '.'
		: {extends, unwrap('$3')}.

export_definition -> export '(' '[' export_list ']' ')' '.'
		: {export, '$4'}.

export_list -> identifier '/' integer
		: [{unwrap('$1'), unwrap('$3')}]. 
export_list -> identifier '/' integer ',' export_list
		: [{unwrap('$1'), unwrap('$3')} | '$5'].

class_attributes -> 'class_attributes.' attributes_definition_list 
		: {unwrap('$1'), '$2'}.

attributes_definition_list -> attributes_definition : ['$1'].
attributes_definition_list -> attributes_definition attributes_definition_list
				:['$1' | '$2'].

attributes_definition -> identifier '=' value : {unwrap('$1'), '$3'}. 

value -> integer	: unwrap('$1').
value -> identifier	: unwrap('$1').
value -> float		: unwrap('$1').	
value -> text		: unwrap('$1').

class_methods -> 'class_methods.' : unwrap('$1'). 

Erlang code.

unwrap({_, _, Value})	-> Value;
unwrap({Value, _})	-> Value.
