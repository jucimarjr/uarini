-module(uarini_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 547).

-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,abstract/1,tokens/1,tokens/2]).
-export([abstract/2, package_segments/1]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).
-export([set_line/2,get_attribute/2,get_attributes/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-export_type([abstract_clause/0, abstract_expr/0, abstract_form/0,
              error_info/0]).

-type abstract_clause() :: term().
-type abstract_expr() :: term().
-type abstract_form() :: term().
-type error_description() :: term().
-type error_info() :: {erl_scan:line(), module(), error_description()}.
-type token() :: {Tag :: atom(), Line :: erl_scan:line()}.

%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

-define(mkop2(L, OpPos, R),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).

-define(mkop1(OpPos, A),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,A}
        end).

%% keep track of line info in tokens
-define(line(Tup), element(2, Tup)).

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo} when
      Tokens :: [token()],
      AbsForm :: abstract_form(),
      ErrorInfo :: error_info().
parse_form([{'-',L1},{atom,L2,spec}|Tokens]) ->
    parse([{'-',L1},{'spec',L2}|Tokens]);
parse_form([{'-',L1},{atom,L2,callback}|Tokens]) ->
    parse([{'-',L1},{'callback',L2}|Tokens]);
parse_form(Tokens) ->
    parse(Tokens).

-spec parse_exprs(Tokens) -> {ok, ExprList} | {error, ErrorInfo} when
      Tokens :: [token()],
      ExprList :: [abstract_expr()],
      ErrorInfo :: error_info().
parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,_} = Err -> Err
    end.

-spec parse_term(Tokens) -> {ok, Term} | {error, ErrorInfo} when
      Tokens :: [token()],
      Term :: term(),
      ErrorInfo :: error_info().
parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    try normalise(Expr) of
		Term -> {ok,Term}
	    catch
		_:_R -> {error,{?line(Expr),?MODULE,"bad term"}}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{?line(E2),?MODULE,"bad term"}};
	{error,_} = Err -> Err
    end.

-type attributes() :: 'export' | 'file' | 'import' | 'module'
		    | 'opaque' | 'record' | 'type'.

build_typed_attribute({atom,La,record},
		      {typed_record, {atom,_Ln,RecordName}, RecTuple}) ->
    {attribute,La,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,La,Attr},
                      {type_def, {call,_,{atom,_,TypeName},Args}, Type})
  when Attr =:= 'type' ; Attr =:= 'opaque' ->
    case lists:all(fun({var, _, _}) -> true;
                      (_)           -> false
                   end, Args) of
        true -> {attribute,La,Attr,{TypeName,Type,Args}};
        false -> error_bad_decl(La, Attr)
    end;
build_typed_attribute({atom,La,Attr},_) ->
    case Attr of
        record -> error_bad_decl(La, record);
        type   -> error_bad_decl(La, type);
	opaque -> error_bad_decl(La, opaque);
        _      -> ret_err(La, "bad attribute")
    end.

build_type_spec({Kind,La}, {SpecFun, TypeSpecs})
  when (Kind =:= spec) or (Kind =:= callback) ->
    NewSpecFun =
	case SpecFun of
	    {atom, _, Fun} ->
		{Fun, find_arity_from_specs(TypeSpecs)};
	    {{atom,_, Mod}, {atom,_, Fun}} ->
		{Mod,Fun,find_arity_from_specs(TypeSpecs)};
	    {{atom, _, Fun}, {integer, _, Arity}} ->
		%% Old style spec. Allow this for now.
		{Fun,Arity};
	    {{atom,_, Mod}, {atom, _, Fun}, {integer, _, Arity}} ->
		%% Old style spec. Allow this for now.
		{Mod,Fun,Arity}
	    end,
    {attribute,La,Kind,{NewSpecFun, TypeSpecs}}.

find_arity_from_specs([Spec|_]) ->
    %% Use the first spec to find the arity. If all are not the same,
    %% erl_lint will find this.
    Fun = case Spec of
	      {type, _, bounded_fun, [F, _]} -> F;
	      {type, _, 'fun', _} = F -> F
	  end,
    {type, _, 'fun', [{type, _, product, Args},_]} = Fun,
    length(Args).

build_def(LHS, Types) ->
    IsSubType = {atom, ?line(LHS), is_subtype},
    {type, ?line(LHS), constraint, [IsSubType, [LHS, Types]]}.

lift_unions(T1, {type, _La, union, List}) ->
    {type, ?line(T1), union, [T1|List]};
lift_unions(T1, T2) ->
    {type, ?line(T1), union, [T1, T2]}.

skip_paren({paren_type,_L,[Type]}) ->
    skip_paren(Type);
skip_paren(Type) ->
    Type.

build_gen_type({atom, La, tuple}) ->
    {type, La, tuple, any};
build_gen_type({atom, La, Name}) ->
    {type, La, Name, []}.

build_bin_type([{var, _, '_'}|Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], Int) ->
    skip_paren(Int);
build_bin_type([{var, La, _}|_], _) ->
    ret_err(La, "Bad binary type").

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Line,module,Module}
%%	{attribute,Line,export,Exports}
%%	{attribute,Line,import,Imports}
%%	{attribute,Line,record,{Name,Inits}}
%%	{attribute,Line,file,{Name,Line}}
%%	{attribute,Line,Name,Val}

build_attribute({atom,La,module}, Val) ->
    case Val of
	[{atom,_Lm,Module}] ->
	    {attribute,La,module,Module};
	[{atom,_Lm,Module},ExpList] ->
	    {attribute,La,module,{Module,var_list(ExpList)}};
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,Module}
	    end;
	[Name,ExpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,{Module,var_list(ExpList)}}
	    end;
	_Other ->
	    error_bad_decl(La, module)
    end;
build_attribute({atom,La,export}, Val) ->
    case Val of
	[ExpList] ->
	    {attribute,La,export,farity_list(ExpList)};
	_Other -> error_bad_decl(La, export)
    end;
build_attribute({atom,La,import}, Val) ->
    case Val of
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,Module}
	    end;
	[{atom,_Lm,Mod},ImpList] ->
	    {attribute,La,import,{Mod,farity_list(ImpList)}};
	[Name, ImpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,{Module,farity_list(ImpList)}}
	    end;
	_Other -> error_bad_decl(La, import)
    end;
build_attribute({atom,La,record}, Val) ->
    case Val of
	[{atom,_Ln,Record},RecTuple] ->
	    {attribute,La,record,{Record,record_tuple(RecTuple)}};
	_Other -> error_bad_decl(La, record)
    end;
build_attribute({atom,La,file}, Val) ->
    case Val of
	[{string,_Ln,Name},{integer,_Ll,Line}] ->
	    {attribute,La,file,{Name,Line}};
	_Other -> error_bad_decl(La, file)
    end;
build_attribute({atom,La,Attr}, Val) ->
    case Val of
	[Expr0] ->
	    Expr = attribute_farity(Expr0),
	    {attribute,La,Attr,term(Expr)};
	_Other -> ret_err(La, "bad attribute")
    end.

var_list({cons,_Lc,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    ret_err(?line(Other), "bad variable list").

attribute_farity({cons,L,H,T}) ->
    {cons,L,attribute_farity(H),attribute_farity(T)};
attribute_farity({tuple,L,Args0}) ->
    Args = attribute_farity_list(Args0),
    {tuple,L,Args};
attribute_farity({op,L,'/',{atom,_,_}=Name,{integer,_,_}=Arity}) ->
    {tuple,L,[Name,Arity]};
attribute_farity(Other) -> Other.

attribute_farity_list(Args) ->
    [attribute_farity(A) || A <- Args].

-spec error_bad_decl(integer(), attributes()) -> no_return().

error_bad_decl(L, S) ->
    ret_err(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,_La,A},{integer,_Li,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    ret_err(?line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    ret_err(?line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 =
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, La, _} ->
                case has_undefined(TypeInfo) of
                    false ->
                        TypeInfo2 = maybe_add_paren(TypeInfo),
                        lift_unions(abstract(undefined, La), TypeInfo2);
                    true ->
                        TypeInfo
                end
	end,
    [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    ret_err(?line(Other), "bad record field");
record_fields([]) -> [].

has_undefined({atom,_,undefined}) ->
    true;
has_undefined({ann_type,_,[_,T]}) ->
    has_undefined(T);
has_undefined({paren_type,_,[T]}) ->
    has_undefined(T);
has_undefined({type,_,union,Ts}) ->
    lists:any(fun has_undefined/1, Ts);
has_undefined(_) ->
    false.

maybe_add_paren({ann_type,L,T}) ->
    {paren_type,L,[{ann_type,L,T}]};
maybe_add_paren(T) ->
    T.

term(Expr) ->
    try normalise(Expr)
    catch _:_R -> ret_err(?line(Expr), "bad attribute")
    end.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,?line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_rule([Clause]) -> {rule,Line,Name,Arity,[Clause]'}

build_rule(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {rule,?line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',Line,{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(
         fun (
         {clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
		 {clause,L,As,G,B};
         ({abst_clause,L,N,As,[]}) when N =:= Name ->
         {abst_clause,L,As};
         ({abst_clause,L,_N,_As,_G}) ->
         ret_err(L, "invalid abstract method");
	     ({clause,L,_N,_As,_G,_B}) ->
		 ret_err(L, "head mismatch") end, Cs).

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

-spec ret_err(_, _) -> no_return().
ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].

%%  Convert between the abstract form of a term and a term.

-spec normalise(AbsTerm) -> Data when
      AbsTerm :: abstract_expr(),
      Data :: term().
normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
	error -> erlang:error({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

-spec abstract(Data) -> AbsTerm when
      Data :: term(),
      AbsTerm :: abstract_expr().
abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer,Line,T};
abstract(T, Line) when is_float(T) -> {float,Line,T};
abstract(T, Line) when is_atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when is_bitstring(B) ->
    {bin, Line, [abstract_byte(Byte, Line) || Byte <- bitstring_to_list(B)]};
abstract([C|T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when is_tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%%  Generate a list of tokens representing the abstract term.

-spec tokens(AbsTerm) -> Tokens when
      AbsTerm :: abstract_expr(),
      Tokens :: [token()].
tokens(Abs) ->
    tokens(Abs, []).

-spec tokens(AbsTerm, MoreTokens) -> Tokens when
      AbsTerm :: abstract_expr(),
      MoreTokens :: [token()],
      Tokens :: [token()].
tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, ?line(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = ?line(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, ?line(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

-type pre_op() :: 'catch' | '+' | '-' | 'bnot' | 'not' | '#'.

-spec preop_prec(pre_op()) -> {0 | 600 | 700, 100 | 700 | 800}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

-spec func_prec() -> {800,700}.

func_prec() -> {800,700}.

-spec max_prec() -> 1000.

max_prec() -> 1000.

%%% [Experimental]. The parser just copies the attributes of the
%%% scanner tokens to the abstract format. This design decision has
%%% been hidden to some extent: use set_line() and get_attribute() to
%%% access the second element of (almost all) of the abstract format
%%% tuples. A typical use is to negate line numbers to prevent the
%%% compiler from emitting warnings and errors. The second element can
%%% (of course) be set to any value, but then these functions no
%%% longer apply. To get all present attributes as a property list
%%% get_attributes() should be used.

set_line(L, F) ->
    erl_scan:set_attribute(line, L, F).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).

get_attributes(L) ->
    erl_scan:attributes_info(L).

%%% Uarini Specific Code
abstract_method({clause,Line,Name,Args,Guards,[]}) ->
    {abst_clause, Line,Name,Args,Guards};
abstract_method(FunNode) ->
    FunNode.

-file("/usr/local/lib/erlang/lib/parsetools-2.0.7/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 795).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_421(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_438(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_455(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_457(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_464(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_465(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_466(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_469(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_477(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_478(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_482(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_490(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_491(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_493(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_495(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_496(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_498(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_501(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, class_attributes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, class_methods, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_0(S, final, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, private, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, protected, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, public, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, static, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_rule(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 497, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_rule_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 496, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(S, final, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, static, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_oo_modifier(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_oo_attribute(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 492, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 490, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_oo_attributes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_oo_modifier(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_function(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 485, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_function_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 484, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 483, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_15(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, callback, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_16(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 313, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_17(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_oo_access(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_oo_visibility(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_oo_visibility(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_oo_visibility(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(S, final, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_oo_access(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_oo_var(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_25(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_700(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_700(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_oo_var(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_900(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_800(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_700(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_600(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_500(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_400(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_300(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_200(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_160(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_150(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_100(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_exprs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_57: see yeccpars2_25

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_prefix_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_prefix_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_64: see yeccpars2_25

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_prefix_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_66: see yeccpars2_25

%% yeccpars2_67: see yeccpars2_25

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_71: see yeccpars2_25

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_prefix_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_75(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_strings(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_77: see yeccpars2_25

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_try_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_85: see yeccpars2_25

yeccpars2_86(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_87: see yeccpars2_25

yeccpars2_88(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 yeccpars2_94(94, Cat, [88 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_89(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_90(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_cr_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_91: see yeccpars2_25

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_cr_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_try_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccpars2_100(_S, Cat, [94 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_95: see yeccpars2_25

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_clause_guard(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_guard(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_98: see yeccpars2_25

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_guard(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_cr_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_101: see yeccpars2_25

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_clause_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_104(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_try_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_105(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccpars2_116(116, Cat, [105 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_106(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atomic(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_108: see yeccpars2_25

yeccpars2_109(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2_110(110, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_110(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccpars2_111(_S, Cat, [110 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_try_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_112: see yeccpars2_25

yeccpars2_113(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccpars2_114(114, Cat, [113 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_114(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccpars2_115(_S, Cat, [114 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_try_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccpars2_117(_S, Cat, [116 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_try_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_118: see yeccpars2_86

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_try_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_120: see yeccpars2_25

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_try_catch(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_122(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_try_catch(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_try_catch(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_strings(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_128: see yeccpars2_25

yeccpars2_129(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 yeccpars2_130(130, Cat, [129 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_130(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_receive_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_132: see yeccpars2_25

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_receive_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_134(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_134_(Stack),
 yeccpars2_135(135, Cat, [134 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_135(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_receive_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_137(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_138: see yeccpars2_25

yeccpars2_139(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_140: see yeccpars2_25

yeccpars2_141(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_142(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_lc_exprs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_lc_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_145: see yeccpars2_25

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_lc_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_147: see yeccpars2_25

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_lc_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_149: see yeccpars2_25

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_lc_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_list_comprehension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_query_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_153(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_154(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_if_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccpars2_156(_S, Cat, [155 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_if_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_157: see yeccpars2_25

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_if_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 yeccgoto_if_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_160(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_161(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccgoto_fun_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_162(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_163(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccpars2_172(172, Cat, [163 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_164(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atom_or_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atom_or_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_fun_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_argument_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_argument_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccpars2_173(_S, Cat, [172 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_fun_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_175(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_atom_or_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 yeccgoto_fun_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_integer_or_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_integer_or_var(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_fun_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_fun_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_186: see yeccpars2_25

yeccpars2_187(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_case_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_190_(Stack),
 yeccgoto_expr_max(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_191(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_194: see yeccpars2_25

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_tail(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_196: see yeccpars2_25

yeccpars2_197(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_tail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_199(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_199(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_tail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_bit_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_203(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccpars2_214(214, Cat, [203 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_204(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expr_max(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_205(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_206(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_bin_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_207_(Stack),
 yeccgoto_binary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_bin_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_binary(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_211: see yeccpars2_25

yeccpars2_212(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_binary_comprehension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccpars2_218(_S, Cat, [214 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_215: see yeccpars2_201

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_bit_size_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_opt_bit_size_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_bin_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_219(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_opt_bit_type_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_bit_type_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_222(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_bit_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_bit_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_225: see yeccpars2_219

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_bit_type_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_bit_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_expr_900(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_expr_max(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_231(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_234(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_236(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_record_fields(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_237(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_238(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_record_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_240: see yeccpars2_25

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_242: see yeccpars2_25

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_record_field(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_record_tuple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_248: see yeccpars2_25

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_250(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_251: see yeccpars2_250

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_expr_100(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_expr_100(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_254: see yeccpars2_250

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_expr_150(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_256: see yeccpars2_250

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_257_(Stack),
 yeccgoto_expr_160(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_258: see yeccpars2_250

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_comp_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_267_(Stack),
 yeccgoto_expr_200(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_268: see yeccpars2_250

%% yeccpars2_269: see yeccpars2_250

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_list_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_list_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_add_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_280(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_expr_400(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_281: see yeccpars2_250

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mult_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_expr_500(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_289_(Stack),
 yeccgoto_expr_300(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_function_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_291(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_292: see yeccpars2_201

%% yeccpars2_293: see yeccpars2_201

yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_expr_800(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_295_(Stack),
 yeccgoto_expr_800(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_296_(Stack),
 yeccgoto_expr_900(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_297(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_298(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 300, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_299_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_300(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_301_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_302_(Stack),
 yeccgoto_expr_600(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_303(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_303(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_304(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_305_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_306(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_307_(Stack),
 yeccgoto_record_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_308_(Stack),
 yeccgoto_oo_access(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_309_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_310_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_311(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 yeccpars2_316(316, Cat, [311 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 yeccgoto_clause_args(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_313(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_313_(Stack),
 yeccgoto_oo_var(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_314: see yeccpars2_25

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_315_(Stack),
 yeccgoto_oo_var(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_316(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_(Stack),
 yeccpars2_318(_S, Cat, [316 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_rule_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_318_(Stack),
 yeccgoto_function_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_319: see yeccpars2_25

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_320_(Stack),
 yeccgoto_rule_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_321(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 459, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_322(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_323: see yeccpars2_322

yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_324_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_325(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_326(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_327(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_spec_fun(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr);
yeccpars2_328(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_329(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_330(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 331, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_330_(Stack),
 yeccgoto_spec_fun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_331(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_332(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_333_(Stack),
 yeccgoto_spec_fun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_334(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_335_(Stack),
 yeccgoto_spec_fun(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_336: see yeccpars2_325

yeccpars2_337(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_338(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 451, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 yeccgoto_type_sigs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_339(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type_sig(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_340(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_340(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 352, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 355, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_340(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type_400(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_342(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type_300(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_343(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type_200(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_344(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 431, Ss, Stack, T, Ts, Tzr);
yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_top_type_100(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type_500(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_346(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 428, Ss, Stack, T, Ts, Tzr);
yeccpars2_346(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_top_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_348(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 426, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_top_types(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_349(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_351(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_352(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_353(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_354(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 395, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_355(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_356(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_357(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_359(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_360(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_361(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_364(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_365_(Stack),
 yeccgoto_top_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_367(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_368(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_fun_type_100(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_370(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 372, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_371_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_372(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_373(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr);
yeccpars2_373(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_374: see yeccpars2_352

yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_375_(Stack),
 yeccgoto_fun_type_100(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_376_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_377(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_378(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_379(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_379(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_380(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 382, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_381(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_384(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_384(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_385_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_386_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_387(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 390, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_388_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_389(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_390_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_391(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_392_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_393(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_394(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_395_(Stack),
 yeccgoto_binary_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_396(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 397, Ss, Stack, T, Ts, Tzr);
yeccpars2_396(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_397(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr);
yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_340(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_bin_base_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_400: see yeccpars2_349

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_401_(Stack),
 yeccgoto_bin_unit_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_402(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_403_(Stack),
 yeccgoto_binary_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_404(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_405(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_406(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_407(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr);
yeccpars2_407(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_408_(Stack),
 yeccgoto_binary_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_409_(Stack),
 yeccgoto_binary_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_410: see yeccpars2_352

yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_411_(Stack),
 yeccgoto_fun_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_412(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr);
yeccpars2_412(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_414(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_414(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_415(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_416(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 424, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_417(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 422, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_417_(Stack),
 yeccgoto_field_types(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_418(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 420, Ss, Stack, T, Ts, Tzr);
yeccpars2_418(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_419(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_419_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_420: see yeccpars2_352

yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_421_(Stack),
 yeccgoto_field_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_422(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_field_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_424_(Stack),
 yeccgoto_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_425_(Stack),
 yeccgoto_type_500(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_426: see yeccpars2_352

yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_427_(Stack),
 yeccgoto_top_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_428(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 429, Ss, Stack, T, Ts, Tzr);
yeccpars2_428(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_429: see yeccpars2_352

yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_430_(Stack),
 yeccgoto_fun_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_431: see yeccpars2_364

yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_432_(Stack),
 yeccgoto_top_type_100(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_433: see yeccpars2_364

%% yeccpars2_434: see yeccpars2_364

yeccpars2_435(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_435_(Stack),
 yeccgoto_type_200(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_436(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_436_(Stack),
 yeccgoto_type_300(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_437: see yeccpars2_364

yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_438_(Stack),
 yeccgoto_type_400(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_439(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr);
yeccpars2_439(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_439(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_440_(Stack),
 yeccgoto_type_sig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_441(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr);
yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 yeccgoto_type_guards(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_442(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_443(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 444, Ss, Stack, T, Ts, Tzr);
yeccpars2_443(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_444: see yeccpars2_352

yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_445_(Stack),
 yeccgoto_type_guard(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_446: see yeccpars2_352

yeccpars2_447(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 448, Ss, Stack, T, Ts, Tzr);
yeccpars2_447(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_448_(Stack),
 yeccgoto_type_guard(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_449: see yeccpars2_439

yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_450_(Stack),
 yeccgoto_type_guards(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_451: see yeccpars2_325

yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_452_(Stack),
 yeccgoto_type_sigs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_453(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_453_(Stack),
 yeccgoto_type_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_454_(Stack),
 yeccgoto_type_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_455_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_456_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_457(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr);
yeccpars2_457(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr);
yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_457_(Stack),
 yeccgoto_attr_val(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_458_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_459: see yeccpars2_25

yeccpars2_460(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 480, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_461(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_462(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 467, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_25(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_463: see yeccpars2_352

yeccpars2_464(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_464_(Stack),
 yeccgoto_typed_attr_val(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_465_(Stack),
 yeccgoto_typed_attr_val(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_466(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 479, Ss, Stack, T, Ts, Tzr);
yeccpars2_466(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_467: see yeccpars2_79

yeccpars2_468(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_469(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr);
yeccpars2_469(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_469_(Stack),
 yeccgoto_typed_exprs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_470(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 471, Ss, Stack, T, Ts, Tzr);
yeccpars2_470(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 472, Ss, Stack, T, Ts, Tzr);
yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_470_(Stack),
 yeccgoto_exprs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_471: see yeccpars2_25

%% yeccpars2_472: see yeccpars2_352

yeccpars2_473(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_473_(Stack),
 yeccgoto_typed_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_474(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_474_(Stack),
 yeccgoto_typed_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_475: see yeccpars2_25

yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_476_(Stack),
 yeccgoto_typed_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_477(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_477_(Stack),
 yeccgoto_typed_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_478(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_478_(Stack),
 yeccgoto_typed_record_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_479(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_479_(Stack),
 yeccgoto_attr_val(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_480_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_481: see yeccpars2_462

yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_482_(Stack),
 yeccgoto_attr_val(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_483_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_484_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_485(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 487, Ss, Stack, T, Ts, Tzr);
yeccpars2_485(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_486_(Stack),
 yeccgoto_function_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_487: see yeccpars2_181

yeccpars2_488(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_488_(Stack),
 yeccpars2_489(489, Cat, [488 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_489(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_489_(Stack),
 yeccpars2_318(_S, Cat, [489 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_490(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 492, Ss, Stack, T, Ts, Tzr);
yeccpars2_490(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_490(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_491_(Stack),
 yeccgoto_oo_attributes(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_492(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 313, Ss, Stack, T, Ts, Tzr);
yeccpars2_492(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_493(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_493_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_494_(Stack),
 yeccgoto_oo_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_495_(Stack),
 yeccgoto_oo_modifier(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_496_(Stack),
 yeccgoto_form(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_497(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 499, Ss, Stack, T, Ts, Tzr);
yeccpars2_497(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_498_(Stack),
 yeccgoto_rule_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_499: see yeccpars2_181

yeccpars2_500(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_500_(Stack),
 yeccpars2_501(501, Cat, [500 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_501(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_501(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccgoto_add_op(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(269, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(343, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(433, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_add_op(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(433, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_argument_list(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_argument_list(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_argument_list(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_argument_list(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_argument_list(487=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_argument_list(499=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_atom_or_var(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(162, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom_or_var(174, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_atomic(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atomic(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_attr_val(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_attribute(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bin_base_type(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(394, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bin_element(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bin_element(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bin_elements(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bin_elements(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bin_unit_type(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(393, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bin_unit_type(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(404, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_binary(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(204, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_binary_comprehension(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_comprehension(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_binary_type(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(352=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(355=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(364=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(397=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(400=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(410=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(426=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(431=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(433=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(437=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_binary_type(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bit_expr(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bit_expr(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(203, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bit_size_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bit_type(219, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bit_type(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(221, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bit_type_list(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_bit_type_list(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_case_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause_args(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_args(487, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(488, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_args(499, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(500, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause_body(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(135, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(155=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_body(489=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause_guard(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(113, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(172, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(316, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(488, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_guard(500, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_501(501, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_comp_op(46, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(258, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_cr_clause(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clause(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clause(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clause(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_cr_clauses(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clauses(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clauses(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cr_clauses(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(187, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(185, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(199, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(197, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_457(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(461, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(470, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(470, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(470, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_100(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_100(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_150(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_150(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(49, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_160(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_160(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_200(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_200(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_300(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_300(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(46, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_400(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_400(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_500(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(280, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_500(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(44, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_600(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_600(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_700(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_700(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_800(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(281, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_800(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_900(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(281, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_900(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expr_max(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(281, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expr_max(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_exprs(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_466(466, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exprs(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_type(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(417, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_type(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(417, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_types(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(416, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_types(422=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_form(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fun_clause(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_clause(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fun_clauses(70, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_clauses(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fun_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fun_type(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_type(336, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_type(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fun_type(451, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fun_type_100(367, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(368, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function_call(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_call(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function_clause(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_clause(485, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function_clauses(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_clauses(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_guard(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_guard(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_if_clause(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_clause(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_if_clauses(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(153, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_clauses(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_if_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_integer_or_var(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_lc_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_expr(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_expr(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_lc_exprs(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(141, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_exprs(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_exprs(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_lc_exprs(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list_comprehension(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list_op(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(268, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mult_op(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(281, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(280, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(281, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(342, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(437, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mult_op(436, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(437, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_access(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_access(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_access(490=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_attribute(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_attribute(490, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_attributes(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_attributes(490=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_modifier(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_modifier(490, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(6, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_var(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_var(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_var(490=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oo_visibility(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oo_visibility(490, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_opt_bit_size_expr(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_opt_bit_type_list(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_prefix_op(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(208, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(201, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(281, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(352, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(355, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(364, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(431, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(433, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(434, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(437, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(444, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(463, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(472, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_prefix_op(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_query_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_receive_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_receive_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_record_expr(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(71, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(77, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(87, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(157, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(211, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(250, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(254, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(256, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(281, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(314, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(462, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_expr(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_record_field(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_field(244, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(236, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_record_fields(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_fields(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_record_tuple(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_tuple(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_record_tuple(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rule(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rule_body(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rule_body(501=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rule_clause(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rule_clause(497, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rule_clauses(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rule_clauses(497=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_spec_fun(322, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(325, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_spec_fun(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(325, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_spec_fun(326, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(336, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_strings(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_strings(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_tail(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tail(199=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_top_type(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(352, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(412, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(355, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(410=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(348, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_464(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_top_type_100(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(352=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(355=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(364=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(410=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(426=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(431=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_type_100(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_top_types(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(346, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(361, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(346, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(384, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(381, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(426=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_top_types(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(447, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_try_catch(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_catch(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_try_clause(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_clause(118, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_try_clauses(86, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_clauses(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_try_expr(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_try_expr(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_tuple(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(77=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(87=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(138=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(147=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(157=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(194=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(250=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(281=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(292=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(314=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tuple(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(352=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(355=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(364=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(397=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(400=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(410=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(426=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(431=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(433=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(437=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_200(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(352, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(355, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(364, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(431, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(444, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(463, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_200(472, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_300(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(352, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(355, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(364, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(431, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(434, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(435, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(444, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(463, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_300(472, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_400(340, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(352, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(355, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(364, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(374, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(377, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(426, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(429, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(431, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(433, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(436, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(434, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(444, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(446, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(463, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_400(472, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_500(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(352=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(355=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(364=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(374=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(410=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(426=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(429=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(431=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(433=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(437=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_500(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_guard(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(441, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_guard(449, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(441, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_guards(439=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_guards(449=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_sig(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(338, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_sig(336, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(338, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_sig(451, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(338, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_sigs(325=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_sigs(336, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(337, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_sigs(451=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type_spec(322=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type_spec(323=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_typed_attr_val(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_attr_val(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(460, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_typed_expr(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(469, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_expr(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(469, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_expr(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(469, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_typed_exprs(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(468, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_exprs(471=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_exprs(475=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_typed_record_fields(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typed_record_fields(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 532).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_rule ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 534).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 91).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 87).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { [ ] , __1 }
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 84).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 233).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   build_function ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 235).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_19_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 100).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 99).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8769).
-compile({inline,yeccpars2_24_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 102).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { { atom , ? line ( __1 ) , 'NoType' } , __1 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8778).
-compile({inline,yeccpars2_38_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 104).
yeccpars2_38_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { { atom , ? line ( __1 ) , 'NoType' } , { match , ? line ( __2 ) , __1 , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 483).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8795).
-compile({inline,yeccpars2_81_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 364).
yeccpars2_81_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ? line ( __1 ) , [ ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8804).
-compile({inline,yeccpars2_82_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 365).
yeccpars2_82_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ? line ( __1 ) , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8813).
-compile({inline,yeccpars2_84_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 453).
yeccpars2_84_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_try ( ? line ( __1 ) , __2 , [ ] , __3 )
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_88_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_90_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 416).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 417).
yeccpars2_92_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8845).
-compile({inline,yeccpars2_93_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 451).
yeccpars2_93_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_try ( ? line ( __1 ) , __2 , __4 , __5 )
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_94_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_96_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 244).
yeccpars2_96_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 486).
yeccpars2_97_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 487).
yeccpars2_99_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8885).
-compile({inline,yeccpars2_100_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 420).
yeccpars2_100_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ? line ( __1 ) , [ __1 ] , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 247).
yeccpars2_102_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_104_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 462).
yeccpars2_104_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_105_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_109_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_109_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_110_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_110_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8931).
-compile({inline,yeccpars2_111_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 472).
yeccpars2_111_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   L = ? line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_113_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_114_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_114_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8955).
-compile({inline,yeccpars2_115_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 469).
yeccpars2_115_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   L = ? line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_116_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 8972).
-compile({inline,yeccpars2_117_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 466).
yeccpars2_117_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   L = ? line ( __1 ) ,
    { clause , L , [ { tuple , L , [ { atom , L , throw } , __1 , { var , L , '_' } ] } ] , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 463).
yeccpars2_119_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_121_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 456).
yeccpars2_121_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 458).
yeccpars2_123_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 460).
yeccpars2_125_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { [ ] , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9014).
-compile({inline,yeccpars2_126_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 497).
yeccpars2_126_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { string , ? line ( __1 ) , element ( 3 , __1 ) ++ element ( 3 , __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_129_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9030).
-compile({inline,yeccpars2_131_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 425).
yeccpars2_131_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'receive' , ? line ( __1 ) , [ ] , __3 , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9039).
-compile({inline,yeccpars2_133_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 423).
yeccpars2_133_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'receive' , ? line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_134_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9055).
-compile({inline,yeccpars2_136_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 427).
yeccpars2_136_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'receive' , ? line ( __1 ) , __2 , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 357).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9072).
-compile({inline,yeccpars2_146_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 362).
yeccpars2_146_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { b_generate , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9081).
-compile({inline,yeccpars2_148_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 361).
yeccpars2_148_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { generate , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 358).
yeccpars2_150_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9098).
-compile({inline,yeccpars2_151_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 354).
yeccpars2_151_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { lc , ? line ( __1 ) , __2 , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9107).
-compile({inline,yeccpars2_152_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 476).
yeccpars2_152_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'query' , ? line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 406).
yeccpars2_154_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_155_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9131).
-compile({inline,yeccpars2_156_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 410).
yeccpars2_156_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ? line ( hd ( hd ( __1 ) ) ) , [ ] , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 407).
yeccpars2_158_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9148).
-compile({inline,yeccpars2_159_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 404).
yeccpars2_159_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'if' , ? line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 443).
yeccpars2_161_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_163_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9172).
-compile({inline,yeccpars2_168_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 431).
yeccpars2_168_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ? line ( __1 ) , { function , element ( 3 , __2 ) , element ( 3 , __4 ) } }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9181).
-compile({inline,yeccpars2_170_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 479).
yeccpars2_170_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { [ ] , ? line ( __1 ) }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9190).
-compile({inline,yeccpars2_171_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 480).
yeccpars2_171_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , ? line ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_172_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_173_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 447).
yeccpars2_173_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { Args , Pos } = __1 ,
    { clause , Pos , 'fun' , Args , __2 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9215).
-compile({inline,yeccpars2_178_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 433).
yeccpars2_178_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'fun' , ? line ( __1 ) , { function , __2 , __4 , __6 } }
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 444).
yeccpars2_182_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9232).
-compile({inline,yeccpars2_183_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 435).
yeccpars2_183_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_fun ( ? line ( __1 ) , __2 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9241).
-compile({inline,yeccpars2_184_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 251).
yeccpars2_184_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'catch' , ? line ( __1 ) , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9250).
-compile({inline,yeccpars2_188_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 414).
yeccpars2_188_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'case' , ? line ( __1 ) , __2 , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9259).
-compile({inline,yeccpars2_190_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 309).
yeccpars2_190_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { block , ? line ( __1 ) , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9268).
-compile({inline,yeccpars2_192_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 318).
yeccpars2_192_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nil , ? line ( __1 ) }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9277).
-compile({inline,yeccpars2_193_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 319).
yeccpars2_193_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { cons , ? line ( __1 ) , __2 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9286).
-compile({inline,yeccpars2_195_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 321).
yeccpars2_195_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { nil , ? line ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 322).
yeccpars2_198_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9303).
-compile({inline,yeccpars2_200_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 323).
yeccpars2_200_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { cons , ? line ( __2 ) , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 339).
yeccpars2_203_(__Stack0) ->
 [begin
   default
  end | __Stack0].

-compile({inline,yeccpars2_206_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 329).
yeccpars2_206_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9327).
-compile({inline,yeccpars2_207_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 326).
yeccpars2_207_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { bin , ? line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 330).
yeccpars2_209_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9344).
-compile({inline,yeccpars2_210_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 327).
yeccpars2_210_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { bin , ? line ( __1 ) , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9353).
-compile({inline,yeccpars2_213_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 356).
yeccpars2_213_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { bc , ? line ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_214_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 342).
yeccpars2_214_(__Stack0) ->
 [begin
   default
  end | __Stack0].

-compile({inline,yeccpars2_217_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 338).
yeccpars2_217_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9377).
-compile({inline,yeccpars2_218_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 333).
yeccpars2_218_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { bin_element , ? line ( __1 ) , __1 , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 341).
yeccpars2_220_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 345).
yeccpars2_221_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_222_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 347).
yeccpars2_222_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   element ( 3 , __1 )
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 348).
yeccpars2_224_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { element ( 3 , __1 ) , element ( 3 , __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 344).
yeccpars2_226_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9426).
-compile({inline,yeccpars2_227_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 335).
yeccpars2_227_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop1 ( __1 , __2 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9435).
-compile({inline,yeccpars2_228_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 295).
yeccpars2_228_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __1 ) , { atom , ? line ( __1 ) , '' } , __2 }
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 308).
yeccpars2_230_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9452).
-compile({inline,yeccpars2_232_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 379).
yeccpars2_232_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record , ? line ( __1 ) , element ( 3 , __2 ) , __3 }
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 392).
yeccpars2_236_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 389).
yeccpars2_239_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9477).
-compile({inline,yeccpars2_241_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 395).
yeccpars2_241_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __1 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9486).
-compile({inline,yeccpars2_243_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 396).
yeccpars2_243_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_245_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 393).
yeccpars2_245_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_246_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 390).
yeccpars2_246_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9511).
-compile({inline,yeccpars2_247_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 377).
yeccpars2_247_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_index , ? line ( __1 ) , element ( 3 , __2 ) , __4 }
  end | __Stack].

-compile({inline,yeccpars2_249_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 484).
yeccpars2_249_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9528).
-compile({inline,yeccpars2_252_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 254).
yeccpars2_252_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { match , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9537).
-compile({inline,yeccpars2_253_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 255).
yeccpars2_253_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9546).
-compile({inline,yeccpars2_255_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 258).
yeccpars2_255_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9555).
-compile({inline,yeccpars2_257_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 261).
yeccpars2_257_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9564).
-compile({inline,yeccpars2_267_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 265).
yeccpars2_267_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9573).
-compile({inline,yeccpars2_280_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 273).
yeccpars2_280_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9582).
-compile({inline,yeccpars2_288_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 277).
yeccpars2_288_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9591).
-compile({inline,yeccpars2_289_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 269).
yeccpars2_289_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( __1 , __2 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9600).
-compile({inline,yeccpars2_290_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 401).
yeccpars2_290_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { call , ? line ( __1 ) , __1 , element ( 1 , __2 ) }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9609).
-compile({inline,yeccpars2_294_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 289).
yeccpars2_294_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { oo_remote , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9618).
-compile({inline,yeccpars2_295_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 291).
yeccpars2_295_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { remote , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9627).
-compile({inline,yeccpars2_296_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 297).
yeccpars2_296_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __2 ) , __1 , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9636).
-compile({inline,yeccpars2_299_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 383).
yeccpars2_299_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record , ? line ( __2 ) , __1 , element ( 3 , __3 ) , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9645).
-compile({inline,yeccpars2_301_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 381).
yeccpars2_301_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __2 ) , __1 , element ( 3 , __3 ) , __5 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9654).
-compile({inline,yeccpars2_302_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 281).
yeccpars2_302_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop1 ( __1 , __2 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9663).
-compile({inline,yeccpars2_305_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 387).
yeccpars2_305_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record , ? line ( __2 ) , __1 , element ( 3 , __3 ) , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9672).
-compile({inline,yeccpars2_307_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 385).
yeccpars2_307_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { record_field , ? line ( __2 ) , __1 , element ( 3 , __3 ) , __5 }
  end | __Stack].

-compile({inline,yeccpars2_308_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 98).
yeccpars2_308_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __2 ]
  end | __Stack].

-compile({inline,yeccpars2_309_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 82).
yeccpars2_309_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_310_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 80).
yeccpars2_310_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_311_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_311_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_312_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 242).
yeccpars2_312_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   element ( 1 , __1 )
  end | __Stack].

-compile({inline,yeccpars2_313_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 103).
yeccpars2_313_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9728).
-compile({inline,yeccpars2_315_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 105).
yeccpars2_315_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , { match , ? line ( __3 ) , __2 , __4 } }
  end | __Stack].

-compile({inline,yeccpars2_316_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_316_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9744).
-compile({inline,yeccpars2_317_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 538).
yeccpars2_317_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { clause , ? line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9753).
-compile({inline,yeccpars2_318_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 239).
yeccpars2_318_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   abstract_method ( { clause , ? line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 } )
  end | __Stack].

-compile({inline,yeccpars2_320_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 540).
yeccpars2_320_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_324_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 110).
yeccpars2_324_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_type_spec ( __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_330_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 117).
yeccpars2_330_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_333_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 121).
yeccpars2_333_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_335_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 120).
yeccpars2_335_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_338_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 135).
yeccpars2_338_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_348_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 149).
yeccpars2_348_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9818).
-compile({inline,yeccpars2_362_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 188).
yeccpars2_362_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , tuple , [ ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9827).
-compile({inline,yeccpars2_363_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 189).
yeccpars2_363_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , tuple , __2 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9836).
-compile({inline,yeccpars2_365_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 152).
yeccpars2_365_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { ann_type , ? line ( __1 ) , [ __1 , __3 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9845).
-compile({inline,yeccpars2_371_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 195).
yeccpars2_371_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , 'fun' , [ ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9854).
-compile({inline,yeccpars2_375_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 199).
yeccpars2_375_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , 'fun' ,
    [ { type , ? line ( __1 ) , any } , __5 ] }
  end | __Stack].

-compile({inline,yeccpars2_376_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 196).
yeccpars2_376_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9872).
-compile({inline,yeccpars2_382_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 180).
yeccpars2_382_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { remote_type , ? line ( __1 ) ,
    [ __1 , __3 , [ ] ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9882).
-compile({inline,yeccpars2_383_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 182).
yeccpars2_383_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { remote_type , ? line ( __1 ) ,
    [ __1 , __3 , __5 ] }
  end | __Stack].

-compile({inline,yeccpars2_385_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 177).
yeccpars2_385_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_gen_type ( __1 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9900).
-compile({inline,yeccpars2_386_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 178).
yeccpars2_386_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) ,
    normalise ( __1 ) , __3 }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9910).
-compile({inline,yeccpars2_388_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 184).
yeccpars2_388_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , nil , [ ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9919).
-compile({inline,yeccpars2_390_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 185).
yeccpars2_390_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , list , [ __2 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9928).
-compile({inline,yeccpars2_392_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 186).
yeccpars2_392_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) ,
    nonempty_list , [ __2 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9938).
-compile({inline,yeccpars2_395_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 215).
yeccpars2_395_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , binary ,
    [ abstract ( 0 , ? line ( __1 ) ) ,
    abstract ( 0 , ? line ( __1 ) ) ] }
  end | __Stack].

-compile({inline,yeccpars2_398_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 225).
yeccpars2_398_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_bin_type ( [ __1 ] , __3 )
  end | __Stack].

-compile({inline,yeccpars2_401_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 227).
yeccpars2_401_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_bin_type ( [ __1 , __3 ] , __5 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9965).
-compile({inline,yeccpars2_403_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 218).
yeccpars2_403_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , binary ,
    [ __2 , abstract ( 0 , ? line ( __1 ) ) ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9975).
-compile({inline,yeccpars2_408_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 223).
yeccpars2_408_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , binary , [ __2 , __4 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9984).
-compile({inline,yeccpars2_409_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 220).
yeccpars2_409_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , binary ,
    [ abstract ( 0 , ? line ( __1 ) ) , __2 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 9994).
-compile({inline,yeccpars2_411_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 203).
yeccpars2_411_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , 'fun' ,
    [ { type , ? line ( __1 ) , product , [ ] } , __4 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10004).
-compile({inline,yeccpars2_413_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 174).
yeccpars2_413_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { paren_type , ? line ( __2 ) , [ __2 ] }
  end | __Stack].

-compile({inline,yeccpars2_417_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 209).
yeccpars2_417_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10021).
-compile({inline,yeccpars2_419_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 190).
yeccpars2_419_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , record , [ __2 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10030).
-compile({inline,yeccpars2_421_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 212).
yeccpars2_421_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , field_type ,
    [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_423_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 210).
yeccpars2_423_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10048).
-compile({inline,yeccpars2_424_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 191).
yeccpars2_424_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) ,
    record , [ __2 | __4 ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10058).
-compile({inline,yeccpars2_425_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 171).
yeccpars2_425_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop1 ( __1 , skip_paren ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_427_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 150).
yeccpars2_427_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10075).
-compile({inline,yeccpars2_430_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 206).
yeccpars2_430_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , 'fun' ,
    [ { type , ? line ( __1 ) , product , __2 } , __5 ] }
  end | __Stack].

-compile({inline,yeccpars2_432_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 156).
yeccpars2_432_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lift_unions ( __1 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10093).
-compile({inline,yeccpars2_435_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 158).
yeccpars2_435_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , range ,
    [ skip_paren ( __1 ) ,
    skip_paren ( __3 ) ] }
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10104).
-compile({inline,yeccpars2_436_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 163).
yeccpars2_436_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( skip_paren ( __1 ) ,
    __2 , skip_paren ( __3 ) )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10114).
-compile({inline,yeccpars2_438_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 167).
yeccpars2_438_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ? mkop2 ( skip_paren ( __1 ) ,
    __2 , skip_paren ( __3 ) )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10124).
-compile({inline,yeccpars2_440_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 139).
yeccpars2_440_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , bounded_fun ,
    [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_441_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 142).
yeccpars2_441_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_445_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 147).
yeccpars2_445_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_def ( __1 , __3 )
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10150).
-compile({inline,yeccpars2_448_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 145).
yeccpars2_448_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type , ? line ( __1 ) , constraint ,
    [ __1 , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_450_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 143).
yeccpars2_450_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_452_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 136).
yeccpars2_452_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_453_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 114).
yeccpars2_453_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_454_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 113).
yeccpars2_454_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_455_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 111).
yeccpars2_455_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_type_spec ( __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_456_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 108).
yeccpars2_456_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_typed_attribute ( __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_457_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 229).
yeccpars2_457_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_458_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 107).
yeccpars2_458_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_attribute ( __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_464_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 124).
yeccpars2_464_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { type_def , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_465_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 123).
yeccpars2_465_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { typed_record , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_469_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 128).
yeccpars2_469_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_470_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 483).
yeccpars2_470_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_473_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 133).
yeccpars2_473_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { typed , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_474_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 130).
yeccpars2_474_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_476_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 129).
yeccpars2_476_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_477_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 131).
yeccpars2_477_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.erl", 10288).
-compile({inline,yeccpars2_478_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 126).
yeccpars2_478_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { tuple , ? line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,yeccpars2_479_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 231).
yeccpars2_479_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __4 ]
  end | __Stack].

-compile({inline,yeccpars2_480_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 109).
yeccpars2_480_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   build_typed_attribute ( __2 , __4 )
  end | __Stack].

-compile({inline,yeccpars2_482_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 230).
yeccpars2_482_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_483_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 77).
yeccpars2_483_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_484_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 78).
yeccpars2_484_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_486_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 236).
yeccpars2_486_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_488_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_488_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_489_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 248).
yeccpars2_489_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_491_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 85).
yeccpars2_491_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_493_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 81).
yeccpars2_493_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_494_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 88).
yeccpars2_494_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_495_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 92).
yeccpars2_495_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __2
  end | __Stack].

-compile({inline,yeccpars2_496_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 79).
yeccpars2_496_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_498_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 535).
yeccpars2_498_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_500_/1}).
-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 245).
yeccpars2_500_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].


-file("/Users/jr/Dropbox/DSc/Src/workspace/uarini/src/uarini_parse.yrl", 1154).
