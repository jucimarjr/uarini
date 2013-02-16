-module(message_store).
-author('elmiliox@gmail.com').
-vsn(1).

-include_lib("stdlib/include/qlc.hrl").
-compile([export_all]).

-define(SERVER, message_store).
-record(chat_message, {addressee, body, created_on}).

save_message(Addressee, Body) ->
    global:send(?SERVER, {save_msg, Addressee, Body}).

find_messages(Addressee) ->
    global:send(?SERVER, {find_msgs, Addressee, self()}),
    receive
        {ok, Messages} -> Messages
    end.

start() ->
    server_util:start(?SERVER, {message_store, run, [true]}).

stop() ->
    server_util:stop(?SERVER).

run(FirstTime) ->
    if
        FirstTime == true ->
            init_store(),
            run(false);
        true ->
            receive
                {save_msg, Addressee, Body} ->
                    store_message(Addressee, Body),
                    run(FirstTime);
                {find_msgs, Addressee, Pid} ->
                    Messages = get_messages(Addressee),
                    Pid ! {ok, Messages},
                    run(FirstTime);
                shutdown ->
                    mnesia:stop(),
                    io:format("Shutting down...~n")
            end
    end,
    ok.

delete_messages(Messages) ->
    F = fun() ->
            lists:foreach(
                fun(Msg) -> mnesia:delete_object(Msg) end,
                Messages)
         end,
    mnesia:transaction(F).

get_messages(Addressee) ->
    F = fun() ->
            Query = qlc:q([M || M <- mnesia:table(chat_message),
                            M#chat_message.addressee =:= Addressee]),
            Results = qlc:e(Query),
            delete_messages(Results),
            [M#chat_message.body || M <- Results]
        end,
    {atomic, Messages} = mnesia:transaction(F),
    Messages.

store_message(Addressee, Body) ->
    F = fun() ->
            {_, CreatedOn, _} = erlang:now(),
            mnesia:write(
                #chat_message{
                    addressee=Addressee,
                    body=Body,
                    created_on=CreatedOn})
        end,
    mnesia:transaction(F).

init_store() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try
        mnesia:table_info(chat_message, type)
    catch
        exit: _ ->
            mnesia:create_table(chat_message, [
                {attributes, record_info(fields, chat_message)},
                {type, bag},
                {disc_copies, [node()]}])
    end.
