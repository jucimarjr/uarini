-module(chocolateBoiler).
-compile(export_all).

new() ->
    Dict = orddict:new(),
    Dict2 = orddict:store(empty, true, Dict),
    Dict3 = orddict:store(boiled, false, Dict2),
    Dict4 = orddict:store(uniqueInstance, [], Dict3),
    PidOfObject = spawn(chocolateBoiler, object_running, [Dict4]),
    ListOfProcesses = erlang:registered(),
    Return = lists:member(new_object, ListOfProcesses),    
    if
        (Return == false) ->
            erlang:register(new_object, PidOfObject),
            new_object;
        true ->
            PidOfObject
    end.
    
object_running(Dict) ->
    receive
        {update, Key, NewValue} ->
            case orddict:find(Key, Dict) of
            	{ok, KeyValue} ->
                    Dict2 = orddict:store(KeyValue, NewValue, Dict);
	           	_->
                    Dict2 = Dict
            end,
            object_running(Dict2)
    %% (other actions like retrieve_attribute, kill_process etc..)
    end.


get_instance() ->
    ListOfProcesses = erlang:registered(),
    Return = lists:member(unique_instance, ListOfProcesses),
    if
		(Return == false) ->
	    	io:format("Creating unique instance of Chocolate Boiler ~n"),
        	NewPid = new(),
			update_attribute(new_object, uniqueInstance, NewPid),
			erlang:register(unique_instance, NewPid);
		true ->
		  	io:format("")
    end,
    io:format("Returning instance of Chocolate Boiler ~n"),
    unique_instance.

update_attribute(Id, Key, NewValue) ->
    Id ! {update, Key, NewValue}.
