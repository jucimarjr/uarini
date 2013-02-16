-module(message_router).
-author('elmiliox@gmail.com').
-vsn(1).

-define(SERVER, message_router).

-compile(export_all).

start() ->
    server_util:start(?SERVER, {?MODULE, route_messages, [dict:new()]}),
    message_store:start().

stop() ->
    server_util:stop(?SERVER),
    message_store:stop().

send_chat_message(Addressee, MessageBody) ->
    global:send(?SERVER, {send_chat_msg, Addressee, MessageBody}).

register_nick(ClientName, ClientPid) ->
    global:send(?SERVER, {register_nick, ClientName, ClientPid}).

unregister_nick(ClientName) ->
    global:send(?SERVER, {unregister_nick, ClientName}).

route_messages(Clients) ->
    receive
        {send_chat_msg, ClientName, MessageBody} ->
            case dict:find(ClientName, Clients) of
                {ok, ClientPid} ->
                    ClientPid ! {printmsg, MessageBody};
                error ->
                    message_store:save_message(ClientName, MessageBody),
                    io:format("Archived message for ~p~n", [ClientName])
            end,
            route_messages(Clients);
        {register_nick, ClientName, ClientPid} ->
            Messages = message_store:find_messages(ClientName),
            io:format("~p", [Messages]),
            lists:foreach(
                fun(Msg) -> ClientPid ! {printmsg, Msg} end,
                Messages),
            route_messages(dict:store(ClientName, ClientPid, Clients));
        {unregister_nick, ClientName} ->
            case dict:find(ClientName, Clients) of
                {ok, ClientPid} ->
                    ClientPid ! stop,
                    route_messages(dict:erase(ClientName, Clients));
                error ->
                    io:format("Error! Unknown client: ~p~n", [ClientName]),
                    route_messages(Clients)
            end;
        shutdown ->
            io:format("Shutting down ~n");
        Oops ->
            io:format("Warning! Received: ~p~n", [Oops]),
            route_messages(Clients)
    end.
