-module(bike).
-export([new/2, manufacture/0]).

new(WorkShop1, WorkShop2) ->
	Dict = orddict:new(),
	Dict2 = orddict:store(workshop1, WorkShop1, Dict),
	Dict3 = orddict:store(workshop2, WorkShop2, Dict2),
	PidOfObject = spawn(bike, object_running, [Dict3]),
	PidOfObject.
	
manufacture() ->
	io:format("Bike ").

object_running(Dict) ->
	receive
        {Sender, manufacture} ->
			manufacture(),
            case orddict:find(workshop1, Dict) of
            	{ok, Value} ->
                    Value ! {self(), work};
	           	_->
                    io:format("")
            end,
			case orddict:find(workshop2, Dict) of
            	{ok, Value2} ->
                    Value2 ! {self(), work};
	           	_->
                    io:format("")
            end,
            object_running(Dict)
    %% (other actions like retrieve_attribute, kill_process etc..)
    end.
