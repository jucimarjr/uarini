
erlang_class -> class_definition class_attributes class_methods

class_definition -> class_name class_extends class_constructor class_exports
class_name -> '-class' '(' identifier ')' '.' 
class_extends-> '-extends' '(' identifier ')' '.'
class_constructor-> '-contructor' '(' '[' method_declaration_list ']' ')' '.'
class_exports-> '-exports' '(' '[' method_declaration_list ']' ')' '.'

method_declaration_list -> method_declararion
method_declaration_list -> method_declararion ',' method_declaration_list
method_declaration -> identifier '/' integer

class_attributes -> 'class attributes.' attribute_definition_list

attribute_definition_list -> attribute_definition
attribute_definition_list -> attribute_definition attribute_definition_list
attribute_definition -> identifier '=' erlang_argument
class_methods -> 'class methods.' method_definition_list

method_definition_list -> method_definition
method_definition_list -> method_definition method_definition_list

method_definition -> method_signature method_body '.'

method_signature -> identifier '(' argument_list ')' '->'

method_body -> statement_list

statement_list -> statement
statement_list -> statement ',' statement_list

statement -> erlang_statement %% qualquer linha de comando valida em erlang
statement -> oo_get_statement
statement -> oo_set_statement
statement -> oo_new_statement

oo_new_statement -> identifier '=' identifier '::' 'new' '(' argument_list ')'

oo_set_statement -> identifier '::' identifier '=' argument

oo_get_statement -> identifier '::' identifier
oo_get_statement -> identifier '=' identifier '::' identifier

argument_list -> .
argument_list -> argument
argument_list -> argument ',' argument_list

argument -> erlang_argument %% qualquer argumento valido em erlang
argument -> object


