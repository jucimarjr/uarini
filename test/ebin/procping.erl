-module(procping).

-export([loop/3, new/3, send/2]).

new(Data, Parent, R) ->
    ObjectID = ooe:new([{'Pid', []}]),
    ooe:update_attr(ObjectID, 'Pid',
		    spawn(fun () -> loop(R, Data, Parent) end)),
    {procping, ObjectID}.

loop(0, _, Parent) -> Parent ! {finish, self()};
loop(R, Data, Parent) ->
    receive
      {init, Parent, Peer} ->
	  Peer ! {self(), Data}, loop(R - 1, Data, Parent);
      {Peer, Data} ->
	  Peer ! {self(), Data}, loop(R - 1, Data, Parent)
    end.

send(ObjectID, Msg) ->
    ooe:lookup_attr(ObjectID, 'Pid') ! Msg.
