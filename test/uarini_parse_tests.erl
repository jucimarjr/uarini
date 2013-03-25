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
    ?OK(Tokens) = aleppo:scan_tokens(Source),
    [begin
         {L, _C} = element(2,T),
         setelement(2, T, L)
     end || T <- Tokens].

uarini_tag_test_() ->
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

    [?_assertEqual(?OK(Exp1), Form1),
     ?_assertEqual(?OK(Exp2), Form2),
     ?_assertEqual(?OK(Exp3), Form3),
     ?_assertEqual(?OK(Exp4), Form4),
     ?_assertEqual(?OK(Exp5), Form5)].

uarini_markup_test_() ->
    Exp1 = {class_attributes, 1, []},
    Exp2 = {class_methods, 1, []},
    Exp3 = {attributes, 1, []},
    Exp4 = {methods, 1, []},

    Form1 = uarini_parse:parse(get_tokens("class_attributes.")),
    Form2 = uarini_parse:parse(get_tokens("class_methods.")),
    Form3 = uarini_parse:parse(get_tokens("attributes.")),
    Form4 = uarini_parse:parse(get_tokens("methods.")),

    [?_assertEqual(?OK(Exp1), Form1),
     ?_assertEqual(?OK(Exp2), Form2),
     ?_assertEqual(?OK(Exp3), Form3),
     ?_assertEqual(?OK(Exp4), Form4)].

uarini_attribute_test_() ->
    Exp1 = {attributes, 1,
               [{oo_attribute, 1,
                   {var, 1, 'MyVar'}}]},

    Exp2 = {class_attributes, 1,
               [{oo_attribute, 1,
                  {var,1,'X'}, {integer,1,4}},
                {oo_attribute, 1,
                  {var,1,'Y'}, {float,1,3.0}},
                {oo_attribute, 1,
                  {var,1,'Z'}, {integer,1,1}}]},

    Form1 = uarini_parse:parse(
	get_tokens("attributes. MyVar.")),
    Form2 = uarini_parse:parse(
	get_tokens("class_attributes. X = 4; Y = 3.0; Z = 1.")),

    [?_assertEqual(?OK(Exp1), Form1),
     ?_assertEqual(?OK(Exp2), Form2)].
