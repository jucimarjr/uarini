-module(proc).
-export([new/1, send/2]).

new(RightProc) ->
	ObjectID = ooe:new(['Pid']),
	Object = {proc, ObjectID},
	ooe:update_attr(ObjectID, 'Pid', spawn( fun() -> loop(RightProc) end )),
	Object.

loop(RightProc) ->
	receive
	Data ->
	    RightProc ! Data,
	    loop(RightProc)
    end.

send(ObjectID, Msg) ->
	ooe:lookup_attr(ObjectID, 'Pid') ! Msg.
