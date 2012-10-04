-module(cantor).
-compile(export_all).

start_class() ->
	ooe:def_class(cantor),
	ooe:def_import(cantor, caneco),
	ooe:def_constructor(cantor, {new, 1}, fun(A) -> new(A) end),
	ooe:def_method(?MODULE, {spawn_singer, 2}, fun(A, B) -> spawn_singer(A, B) end),	
	ooe:def_method(?MODULE, {sing_verse, 2}, fun(A, B) -> sing_verse(A, B) end).

new(_Key) ->
	ok.

spawn_singer(Bottle, _Key) ->    
	Pid = self(),
	Caneco = ooe:constructor(caneco, {new, 1}, []),
	Pid !
		ooe:method(caneco, {create_verse, 2}, [Bottle], Caneco).

sing_verse(Bottle, _Key) ->
    receive
	{_, Verse} when Bottle == 0 ->
	    io:format(Verse);
	{N, Verse} when Bottle == N ->
	    io:format(Verse),
	    sing_verse(Bottle-1, _Key)
    after 
	3000 ->
	    io:format("Verse not received after 3 seconds - re-starting singer~n"),
	    spawn_singer(Bottle, _Key),
	    sing_verse(Bottle, _Key)
    end.	
