-module(chat_client).
-author('elmiliox@gmail.com').
-vsn(1).

-compile(export_all).

register_nickname(Nickname) ->
    Pid = spawn(?MODULE, handle_messages, [Nickname]),
    message_router:register_nick(Nickname, Pid).

unregister_nickname(Nickname) ->
    message_router:unregister_nick(Nickname).

send_message(Addressee, MessageBody) ->
    message_router:send_chat_message(Addressee, MessageBody).

handle_messages(Nickname) ->
    receive
        {printmsg, MessageBody} ->
            io:format("~p received: ~p~n", [Nickname, MessageBody]),
            handle_messages(Nickname);
        stop ->
            ok
    end.

start_router() ->
    message_router:start().
