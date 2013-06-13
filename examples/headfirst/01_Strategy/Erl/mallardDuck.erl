-module(mallardDuck).
-export([new/0, display/0]).

new() ->
	Dict = orddict:new(),
	Dict2 = orddict:store(quackBehaviour, quack:new(), Dict),
	Dict3 = orddict:store(flyBehaviour, fly:new(), Dict2),
	IdOfObject = spawn(mallardDuck, object_running, [Dict3]),
	IdOfObject.

	
display() ->
	io:format("I'm a real Mallard Duck!~n").

object_running(Dict) ->
	receive
		{Sender, update} ->
			io:format("")
		%%All messages to the object interaction
	end.
