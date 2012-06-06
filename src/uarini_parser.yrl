Nonterminals

erlang_class
class_name oo_definitions
class_extends class_exports class_constructor class_import
class_attributes attributes_definition attributes_definition_list 
identifier_list method_declaration_list method_declaration
class_methods
method_definition_list method_definition method_signature
method_body method_statement_list method_statement
oo_new_statement oo_set_statement oo_get_statement
argument_list argument tuple list erlang_statement terminal
add_expr mult_expr modulus_expr unary_expr
erlang_match.


Terminals

'=' '::' '(' ')' '[' ']' '{' '}' ',' ';' '.' '->' 
'!' 'rem' '*' '/' '+' '-' '&&' '||' '<' '>'
integer float identifier text 
'-class' export extends constructor import null
'class' 'attributes.' 'methods.'.


Rootsymbol erlang_class.


erlang_class -> class_name oo_definitions class_methods
		: ['$1', '$2', '$3'].
erlang_class -> class_name oo_definitions class_attributes class_methods
		: ['$1', '$2', '$3', '$4'].

oo_definitions -> class_exports				: ['$1'].

oo_definitions -> class_exports class_constructor 	: ['$1', '$2'].
oo_definitions -> class_constructor class_exports  	: ['$1', '$2'].

oo_definitions -> class_extends class_exports	 	: ['$1', '$2'].
oo_definitions -> class_exports class_extends  		: ['$1', '$2'].

oo_definitions -> class_import class_exports 		: ['$1', '$2'].
oo_definitions -> class_exports class_import  		: ['$1', '$2'].

oo_definitions -> class_exports class_constructor class_extends
		: ['$1', '$2', '$3'].
oo_definitions -> class_exports class_extends class_constructor
		: ['$1', '$2', '$3'].
oo_definitions -> class_constructor class_exports class_extends
		: ['$1', '$2', '$3'].
oo_definitions -> class_constructor  class_extends class_exports
		: ['$1', '$2', '$3'].
oo_definitions -> class_extends class_exports class_constructor
		: ['$1', '$2', '$3'].
oo_definitions -> class_extends class_constructor class_exports 
		: ['$1', '$2', '$3'].

oo_definitions -> class_exports class_import class_extends
		: ['$1', '$2', '$3'].
oo_definitions -> class_exports class_extends class_import
		: ['$1', '$2', '$3'].
oo_definitions -> class_import class_exports class_extends
		: ['$1', '$2', '$3'].
oo_definitions -> class_import  class_extends class_exports
		: ['$1', '$2', '$3'].
oo_definitions -> class_extends class_exports class_import
		: ['$1', '$2', '$3'].
oo_definitions -> class_extends class_import class_exports 
		: ['$1', '$2', '$3'].

oo_definitions -> class_exports class_constructor class_import
		: ['$1', '$2', '$3'].
oo_definitions -> class_exports class_import class_constructor
		: ['$1', '$2', '$3'].
oo_definitions -> class_constructor class_exports class_import
		: ['$1', '$2', '$3'].
oo_definitions -> class_constructor  class_import class_exports
		: ['$1', '$2', '$3'].
oo_definitions -> class_import class_exports class_constructor
		: ['$1', '$2', '$3'].
oo_definitions -> class_import class_constructor class_exports 
		: ['$1', '$2', '$3'].

oo_definitions -> class_exports class_constructor class_extends
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_exports class_constructor class_import
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_exports class_extends class_constructor
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_exports class_extends class_import
			class_constructor :['$1', '$2', '$3', '$4'].
oo_definitions -> class_exports class_import class_constructor
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_exports class_import class_extends
			class_constructor :['$1', '$2', '$3', '$4'].

oo_definitions -> class_constructor class_exports class_extends
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_constructor class_exports class_import
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_constructor class_extends class_exports
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_constructor class_extends class_import
			class_exports :['$1', '$2', '$3', '$4'].
oo_definitions -> class_constructor class_import class_exports
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_constructor class_import class_extends
			class_exports :['$1', '$2', '$3', '$4'].

oo_definitions -> class_extends class_constructor class_exports
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_extends class_constructor class_import
			class_exports :['$1', '$2', '$3', '$4'].
oo_definitions -> class_extends class_exports class_constructor
			class_import :['$1', '$2', '$3', '$4'].
oo_definitions -> class_extends class_exports class_import
			class_constructor :['$1', '$2', '$3', '$4']. 
oo_definitions -> class_extends class_import class_constructor
			class_exports :['$1', '$2', '$3', '$4'].
oo_definitions -> class_extends class_import class_exports
			class_constructor :['$1', '$2', '$3', '$4'].

oo_definitions -> class_import class_constructor class_extends
			class_exports :['$1', '$2', '$3', '$4'].
oo_definitions -> class_import class_constructor class_exports
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_import class_extends class_constructor
			class_exports :['$1', '$2', '$3', '$4'].
oo_definitions -> class_import class_extends class_exports
			class_constructor :['$1', '$2', '$3', '$4'].
oo_definitions -> class_import class_exports class_constructor
			class_extends :['$1', '$2', '$3', '$4'].
oo_definitions -> class_import class_exports class_extends
			class_constructor :['$1', '$2', '$3', '$4'].

class_name -> '-class' '(' identifier ')' '.'
		: {class, unwrap('$3')}.

class_constructor -> constructor '(' identifier '/' integer ')' '.'
		: {constructor, unwrap('$3'), unwrap('$5')}.

class_extends -> extends '(' identifier ')' '.'
		: {extends, unwrap('$3')}.

class_exports -> export '(' '[' method_declaration_list ']' ')' '.'	
		: {export, '$4'}.

method_declaration_list -> method_declaration 
		: ['$1']. 
method_declaration_list -> method_declaration ',' method_declaration_list
		: ['$1' | '$3'].

method_declaration  -> identifier '/' integer: {unwrap('$1'), unwrap('$3')}.

class_import -> import '(' '[' identifier_list ']' ')' '.'
		: {import, '$4'}.

identifier_list -> identifier : [unwrap('$1')].
identifier_list -> identifier ',' identifier_list : [unwrap('$1') | ('$3')].

class_attributes -> 'class' 'attributes.' attributes_definition_list 
		: {'class attributes.', '$3', '.'}.

attributes_definition_list -> attributes_definition : ['$1'].
attributes_definition_list -> attributes_definition attributes_definition_list
				:['$1' | '$2'].

attributes_definition -> erlang_match: '$1'.

erlang_match -> identifier '=' add_expr : {unwrap('$1'), '$3'}.

class_methods -> 'class' 'methods.' method_definition_list
			: {'class methods.', '$3','.'}.

method_definition_list -> method_definition: ['$1'].
method_definition_list -> method_definition method_definition_list
			: ['$1' | '$2'].

method_definition -> method_signature method_body '.' 
			: {'$1', '$2'}.

method_signature -> identifier '(' ')' '->'
			: {signature, unwrap('$1'), []}.
method_signature -> identifier '(' argument_list ')' '->'
			: {signature, unwrap('$1'), '$3'}.

method_body -> 	method_statement_list: '$1'.

method_statement_list -> method_statement : ['$1'].		
method_statement_list -> method_statement ',' method_statement_list : ['$1' |'$3'].
		
method_statement	-> erlang_statement	: '$1'.	
method_statement	-> oo_new_statement	: '$1'.
method_statement	-> oo_set_statement	: '$1'.
method_statement	-> oo_get_statement	: '$1'.

erlang_statement -> terminal			: ['$1'].		
erlang_statement -> terminal erlang_statement	: ['$1' | '$2'].

terminal ->	';' 		: unwrap('$1').
terminal ->	'->'  		: unwrap('$1').
terminal ->	'!'  		: unwrap('$1').
terminal ->	'&&'  		: unwrap('$1').
terminal ->	'||'  		: unwrap('$1').
terminal ->	'<'  		: unwrap('$1').
terminal ->	'>'  		: unwrap('$1').
terminal ->	argument	: '$1'.
terminal -> 	erlang_match	: '$1'.

oo_new_statement -> 
	identifier '=' identifier '::' add_expr:
	{oo_new, {unwrap('$1'), unwrap('$3'), '$5'}}.
oo_set_statement -> 
	identifier '::' identifier '=' add_expr:
	{oo_set, {unwrap('$1'), unwrap('$3'), '$5'}}.
oo_get_statement -> 
	identifier '::' add_expr:
	{oo_get, {unwrap('$1'), '$3'}}.

add_expr -> mult_expr '+' add_expr	: {'+', '$1', '$3'}.
add_expr -> mult_expr '-' add_expr	: {'-', '$1', '$3'}.
add_expr -> mult_expr			: '$1'.

mult_expr -> modulus_expr '*' mult_expr	:{'*', '$1', '$3'}.
mult_expr -> modulus_expr '/' mult_expr	:{'/', '$1', '$3'}.
mult_expr -> modulus_expr		: '$1'.

modulus_expr -> unary_expr 'rem' modulus_expr	:{'rem', '$1', '$3'}.
modulus_expr -> unary_expr			: '$1'.

unary_expr -> '-' argument	: {'-', '$2'}.
unary_expr -> argument		: '$1'.

argument_list -> argument			: ['$1'].
argument_list -> argument ',' argument_list	: ['$1' |'$3'].

argument -> tuple						:'$1'.
argument -> list						:'$1'.
argument -> identifier	 					:'$1'.
argument -> identifier '(' argument_list ')' 			: {unwrap('$1'), '$3'}.
argument -> identifier '(' ')'					: {unwrap('$1'), {none}}.
argument -> integer						: unwrap('$1').
argument -> null						: unwrap('$1').
argument -> float						: unwrap('$1').	
argument -> text						: unwrap('$1').


tuple -> '{' '}'				: {tuple}.
tuple -> '{' argument_list '}'	: {tuple, '$2'}.

list -> '[' ']'					: {list}.
list -> '[' argument_list ']'	: {list, '$2'}.


Erlang code.

unwrap({_, _, Value})	-> Value;
unwrap({Value, _})	-> Value.
