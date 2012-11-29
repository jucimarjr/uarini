%% Autor      : Emiliano Carlos de Moraes Firmino ( emiliano.firmino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Descricao  : Unit tests of uarini_parse

-module(uarini_parse_tests).
-author('emiliano.firmino@gmail.com').
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

get_tokens(Source) ->
    {ok, Tokens, _Lines} = uarini_scan:string(Source),
    Tokens.

uarini_tag_test() ->
    Exp1 = {ok, {attribute,1,class,      name}},
    Exp2 = {ok, {attribute,1,interface,  name}},
    Exp3 = {ok, {attribute,1,extends,    name}},
    Exp4 = {ok, {attribute,1,implements, [c1, c2, c3]}},
    Exp5 = {ok, {attribute,1,constructor, [{new,0},{new,1}]}},
    
    Form1 = uarini_parse:parse(get_tokens("-class(name).")),
    Form2 = uarini_parse:parse(get_tokens("-interface(name).")),
    Form3 = uarini_parse:parse(get_tokens("-extends(name).")),
    Form4 = uarini_parse:parse(get_tokens("-implements([c1,c2,c3]).")),
    Form5 = uarini_parse:parse(get_tokens("-constructor([new/0, new/1]).")),

    [?assertEqual(Exp1, Form1),
     ?assertEqual(Exp2, Form2),
     ?assertEqual(Exp3, Form3),
     ?assertEqual(Exp4, Form4),
     ?assertEqual(Exp5, Form5)].

uarini_markup_test() ->
    Exp1 = {ok, {class_attributes, 1}},
    Exp2 = {ok, {class_methods,    1}},
    
    Form1 = uarini_parse:parse(get_tokens("class_attributes.")),
    Form2 = uarini_parse:parse(get_tokens("class_methods.")),

    [?assertEqual(Exp1, Form1),
     ?assertEqual(Exp2, Form2)].
