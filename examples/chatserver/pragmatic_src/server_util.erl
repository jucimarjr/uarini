-module(server_util).
-vsn(1).
-author('elmiliox@gmail.com').

-compile(export_all).


start(ServerName, {Module, Function, Args}) ->
    global:trans({ServerName, ServerName},
        fun() ->
            case global:whereis_name(ServerName) of
                undefined ->
                    Pid = spawn(Module, Function, Args),
                    global:register_name(ServerName, Pid);
                _ ->
                    ok
            end
    end).

stop(ServerName) ->
    global:trans({ServerName, ServerName},
        fun() ->
            case global:whereis_name(ServerName) of
                undefined ->
                    ok;
                _ ->
                    global:send(ServerName, shutdown)
            end
    end).

