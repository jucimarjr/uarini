%% Autor      : Emiliano Carlos de Moraes Firmino ( emiliano.firmino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Descricao  : Unit tests of uarini_scan

-module(uarini_scan_tests).
-author('emiliano.firmino@gmail.com').
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

-define(SCAN, uarini_scan).
-define(RW_TOKEN(ReservedWord), {ok, [{ReservedWord, 1}], 1}).

reserved_word_test() ->
    ReservedWords = [
        class_attributes,
        class_methods,
        public,
        protected,
        private,
        static,
        final],

    AssertEquals = fun (W)->
        ?assertEqual(?RW_TOKEN(W), ?SCAN:string(atom_to_list(W)))
    end,
    lists:map(AssertEquals, ReservedWords).
