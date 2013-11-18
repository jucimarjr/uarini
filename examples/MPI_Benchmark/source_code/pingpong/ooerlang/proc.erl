-module(proc).
-export([new/3, send/2, destroy/1]).

%% no ooe eh  enviada uma mensagem assim:
%% ObjectID ! {self(), destroy}, que mata o processo que guarda os atributos
%% destroy gerado automaticamente
destroy(ObjectID) ->
	ooe:destroy(ObjectID).

new(Data, Parent, R) ->
	ObjectID = ooe:new(['Pid']), % ObjectID = <0.35.0> = pid
	Object = {proc, ObjectID},
	ooe:update_attr(ObjectID, 'Pid', spawn( fun() -> loop(R, Data, Parent) end )),
	io:format("new: ~w ~w ~w ~n",[ObjectID,Parent, R]),
	Object.

loop(0, _, Parent) ->
    Parent ! {finish, self()};

loop(R, Data, Parent) ->
	receive
		{init, Parent, Peer} ->
			io:format("start: ~w ~w ~w ~w ~n",[self(),Peer,Parent, R]),
	    	Peer ! {self(), Data},
	    	loop(R-1, Data, Parent);
	    	
		{Peer, Data} ->
			io:format("~w ~w ~n",[Peer,R]),
	    	Peer ! {self(), Data},
	    	loop(R-1, Data, Parent)
   end.

%% exemplo de metodo de objeto
send(ObjectID, Msg) ->
    ooe:lookup_attr(ObjectID, 'Pid') ! Msg.
