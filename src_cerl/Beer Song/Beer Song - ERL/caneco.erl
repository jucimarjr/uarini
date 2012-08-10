-module(caneco).
-compile(export_all).

start_class() ->
	ooe:def_class(caneco),
	ooe:def_constructor(caneco, {new, 1}, 
						fun(A) -> new(A) end),
	ooe:def_method(caneco, {create_verse, 2},
							fun(A, B) -> create_verse(A, B) end).

-define(TEMPLATE_0, "~s of beer on the wall, ~s of beer.~nGo to the store and buy some more, 99 bottles of beer on the wall.~n").
-define(TEMPLATE_N, "~s of beer on the wall, ~s of beer.~nTake one down and pass it around, ~s of beer on the wall.~n~n").

new(_Key)->
	ok.

phrase(0, _Key)      -> ["No more bottles", "no more bottles"];
phrase(1, _Key)      -> ["1 bottle", "1 bottle", "no more bottles"];
phrase(2, _Key)      -> ["2 bottles", "2 bottles", "1 bottle"];
phrase(Bottle, _Key) -> lists:duplicate(2, integer_to_list(Bottle) 
										++ " bottles") 
								++ [integer_to_list(Bottle-1) ++ " bottles"].

create_verse(0, _Key) -> 
	{0, io_lib:format(?TEMPLATE_0, phrase(0, _Key))};
create_verse(Bottle, _Key) -> 
	{Bottle, io_lib:format(?TEMPLATE_N, phrase(Bottle, _Key))}.
