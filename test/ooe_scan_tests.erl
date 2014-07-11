%% Autor      : Emiliano Carlos de Moraes Firmino ( emiliano.firmino@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Descricao  : Unit tests of ooe_scan

-module(ooe_scan_tests).
-author('emiliano.firmino@gmail.com').
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

-define(SCAN, ooe_scan).
-define(RW_TOKEN(ReservedWord), {ok, [{ReservedWord, 1}], 1}).
-define(ATOM_TOKEN(Atom),       {ok, [{atom, 1,   Atom}], 1}).


reserved_word_test() ->
    ReservedWords = [
        class_attributes,
        class_methods,
        attributes,
        methods],
    NotReservedWords = [
        public,
        protected,
        private,
        static,
        final],

    AssertReserved = fun (W)->
        ?assertEqual(?RW_TOKEN(W), ?SCAN:string(atom_to_list(W)))
    end,
    AssertAtom = fun (W)->
        ?assertEqual(?ATOM_TOKEN(W), ?SCAN:string(atom_to_list(W)))
    end,

    lists:map(AssertReserved, ReservedWords) ++
    lists:map(AssertAtom,  NotReservedWords).
