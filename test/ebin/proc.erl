-module(proc).

-export([loop/1, new/1, send/2]).

new(RightProc) ->
    ObjectID = ooe:new([{'Pid', []}]),
    ooe:update_attr(ObjectID, 'Pid',
		    spawn(fun () -> loop(RightProc) end)),
    {proc, ObjectID}.

loop(RightProc) ->
    receive Data -> RightProc ! Data, loop(RightProc) end.

send(ObjectID, Msg) ->
    ooe:lookup_attr(ObjectID, 'Pid') ! Msg.
