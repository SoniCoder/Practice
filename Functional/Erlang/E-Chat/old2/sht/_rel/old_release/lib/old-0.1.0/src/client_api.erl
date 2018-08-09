-module(client_api).
-export([con/2, close_server/0]).

-include("headers.hrl").

%%% Client Utility Functions

current_timestamp() -> io_lib:format("[~2..0b:~2..0b:~2..0b]", tuple_to_list(time())).

create_message(Message, Sender) -> create_message(Message, Sender, false).

create_message(Message, Sender, Private) ->
    #message{timestamp=current_timestamp(),name=Sender, text=Message, private=Private}.

printStringList([]) -> ok;
printStringList([H|T]) ->
    printMessage(H),
    printStringList(T).

printList([]) -> ok;
printList([H|T]) ->
    io:format("~p~n", [H]),
    printList(T).

%%% Client API
printMessage(M) ->
        if
            M#message.private == false ->
                    io:format("~-12s ~-10s : ~s", [M#message.timestamp, M#message.name, M#message.text]);
            M#message.private == true ->
                    io:format("~-12s ~-10s : ~s", [M#message.timestamp, "[" ++ M#message.name ++ "]", M#message.text])
        end.


keep_receiving() ->
    receive
        {_, quit} ->
            ok;
        {_, M} ->
            printMessage(M),            
            keep_receiving()
    after infinity -> erlang:error(timeout)
    end.

con(out, Name) ->
    Pid = whereis(chatsv),
    M = gen_server:call(Pid, {subscribe, Name}),
    case M of
        {ok, MList} -> 
            printStringList(MList),
            keep_receiving();
        {error, ErrMsg} ->
            ErrMsg
    end;


con(in, Name) ->
    Pid = whereis(chatsv),
    io:format("Welcome to E-Chat!~n~n"),
    gen_server:call(Pid, {broadcast, create_message(lists:concat([Name, " has entered the server.\n"]), "*Server*")}),
    send_to_server(Name, Pid),
    ok.

send_to_server(Name, Pid) ->
    Text = io:get_line("enter message> "),
    {ok, RE_PRV} = re:compile("/w (\\w+) (.+\\n)"),
    case Text of
        "/l" ++ _ ->
            ClientList = gen_server:call(Pid, {getclist}),
            printList(ClientList);
        "/w" ++ _ -> 
            {match, [ReceiverName | [MainText]]} =  re:run(Text, RE_PRV , [{capture, all_but_first, list}]),
            gen_server:call(Pid, {unicast, create_message(MainText, Name, true), ReceiverName});
        "/q"++ _ ->
            gen_server:call(Pid, {disconnect, Name}),
            gen_server:call(Pid, {broadcast, create_message(lists:concat([Name, " has left the server.\n"]), "*Server*")}),
            exit(normal);
        _ ->
            gen_server:call(Pid, {broadcast, create_message(Text, Name)})
    end,
    send_to_server(Name, Pid).

close_server() ->
    gen_server:call(whereis(chatsv), {terminate}).
