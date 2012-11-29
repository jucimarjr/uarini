%% Autor      : Emiliano Carlos de Moraes Firmino ( emiliano.firmino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Descricao  : Unit tests of uarini_parse

-module(uarini_parse_tests).
-author('emiliano.firmino@gmail.com').
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

-define(OK(RV1, RV2), {ok, RV1, RV2}).
-define(OK(ReturnValue), {ok, ReturnValue}).

get_tokens(Source) ->
    ?OK(Tokens, _Lines) = uarini_scan:string(Source),
    Tokens.

uarini_tag_test() ->
    Exp1 = {attribute, 1, class, name},
    Exp2 = {attribute, 1, extends, name},
    Exp3 = {attribute, 1, interface, name},
    Exp4 = {attribute, 1, implements, [c1, c2, c3]},
    Exp5 = {attribute, 1, constructor, [{new,0},{new,1}]},
    
    Form1 = uarini_parse:parse(get_tokens("-class(name).")),
    Form2 = uarini_parse:parse(get_tokens("-extends(name).")),
    Form3 = uarini_parse:parse(get_tokens("-interface(name).")),
    Form4 = uarini_parse:parse(get_tokens("-implements([c1,c2,c3]).")),
    Form5 = uarini_parse:parse(get_tokens("-constructor([new/0, new/1]).")),

    [?assertEqual(?OK(Exp1), Form1),
     ?assertEqual(?OK(Exp2), Form2),
     ?assertEqual(?OK(Exp3), Form3),
     ?assertEqual(?OK(Exp4), Form4),
     ?assertEqual(?OK(Exp5), Form5)].

uarini_markup_test() ->
    Exp1 = {class_attributes, 1},
    Exp2 = {class_methods, 1},
    
    Form1 = uarini_parse:parse(get_tokens("class_attributes.")),
    Form2 = uarini_parse:parse(get_tokens("class_methods.")),

    [?assertEqual(?OK(Exp1), Form1),
     ?assertEqual(?OK(Exp2), Form2)].

uarini_var_test() ->
    Exp1 = [{[],
             {{atom,1,'NoType'}, {var,1,'MyVar'}
           }}],
    Exp2 = [{[],
             {{atom,1, car}, {var,1,'Fusca'}
           }}],
    Exp3 = [{
        [{public,1}],
	{{atom,1,'NoType'}, {var,1,'MyVar'}}}],
    Exp4 = [{
        [{private,1}],
	{{atom,1, car},{var,1,'Fusca'}}}],
    Exp5 = [{
        [{protected,1},{static,1},{final,1}],
	{{atom,1, car}, {var,1,'Ferrari'} }}],

    Form1 = uarini_parse:parse(get_tokens("MyVar.")),
    Form2 = uarini_parse:parse(get_tokens("car Fusca.")),
    Form3 = uarini_parse:parse(get_tokens("public MyVar.")),
    Form4 = uarini_parse:parse(get_tokens("private car Fusca.")),
    Form5 = uarini_parse:parse(get_tokens("protected static final car Ferrari.")),

    [?assertEqual(?OK(Exp1), Form1),
     ?assertEqual(?OK(Exp2), Form2),
     ?assertEqual(?OK(Exp3), Form3),
     ?assertEqual(?OK(Exp4), Form4),
     ?assertEqual(?OK(Exp5), Form5)].
