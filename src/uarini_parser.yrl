Nonterminals

start_parser grammar
class_definition export_definition 
atributtes_definition constructor_definition
export_list atributtes_list value.

Terminals
'(' ')' '[' ']' '{' '}' ',' '.' '/'
integer float identifier text class constructor atributtes export.

Rootsymbol start_parser.

start_parser -> class_definition grammar: ['$1' | '$2'].

grammar -> export_definition atributtes_definition
		: ['$1', '$2'].
grammar -> atributtes_definition export_definition
		: ['$1', '$2'].
grammar -> export_definition atributtes_definition constructor_definition
		: ['$1', '$2', '$3'].
grammar -> export_definition constructor_definition atributtes_definition
		: ['$1', '$2', '$3'].
grammar -> constructor_definition export_definition atributtes_definition
		: ['$1', '$2', '$3'].
grammar -> atributtes_definition export_definition constructor_definition
		: ['$1', '$2', '$3'].
grammar -> atributtes_definition constructor_definition export_definition 
		: ['$1', '$2', '$3'].
grammar -> constructor_definition atributtes_definition export_definition
		: ['$1', '$2', '$3'].

class_definition -> class '(' identifier ')' '.'
		: {class, unwrap('$3')}.

constructor_definition -> constructor '(' identifier '/' integer ')' '.'
		: {constructor, unwrap('$3'), unwrap('$5')}. 

export_definition -> export '(' '[' export_list ']' ')' '.'
		: {export, '$4'}.

export_list -> identifier '/' integer
		: [{unwrap('$1'), unwrap('$3')}]. 
export_list -> identifier '/' integer ',' export_list
		: [{unwrap('$1'), unwrap('$3')} | '$5'].

atributtes_definition -> atributtes '(' '[' atributtes_list ']' ')' '.'
		: {atributtes, '$4'}.

atributtes_list -> '{' identifier ',' value '}'
			: [{unwrap('$2'), '$4'}]. 
atributtes_list -> '{' identifier ',' value '}' ',' atributtes_list
			: [{unwrap('$2'), '$4'} | '$7'].

value -> integer: unwrap('$1').
value -> float	: unwrap('$1').	
value -> text	: unwrap('$1').

Erlang code.

unwrap({_, _, Value})	-> Value.
