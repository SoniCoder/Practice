%%%%% Abstracted version
-module(echat).

-export([connect/2, start_link/0]).
-export([init/1, handle_call/3]).

-record(message, {timestamp, name, text, private=false}).
-record(state, {msglist, scribers, clientinfo}).

%%% Client API
printMessage(M) ->
        io:format("~-12s ~-10s : ~s", [M#message.timestamp, M#message.name, M#message.text]).

keep_receiving() ->
    receive
        {_, M} ->
            printMessage(M),            
            keep_receiving()
    after infinity -> erlang:error(timeout)
    end.

printStringList([]) -> ok;
printStringList([H|T]) ->
    printMessage(H),
    printStringList(T).

connect(output, Name) ->
    Pid = whereis(chatsv),
    M = my_server:call(Pid, {subscribe, Name}),
    printStringList(M),
    keep_receiving();

connect(input, Name) ->
    Pid = whereis(chatsv),
    io:format("Welcome to E-Chat!~n~n"),
    my_server:call(Pid, {broadcast, #message{timestamp=io_lib:format("[~2..0b:~2..0b:~2..0b]", tuple_to_list(time())),name="*Server* ", text=lists:concat([Name, " has entered the server.\n"])}}),
    send_to_server(Name, Pid),
    ok.

send_to_server(Name, Pid) ->
    Text = io:get_line("enter message> "),
    {ok, RE_PRV} = re:compile("/w (\\w+) (.+\\n)"),
    case Text of
        "/w" ++ _ -> 
            {match, [ReceiverName | [MainText]]} =  re:run(Text, RE_PRV , [{capture, all_but_first, list}]),
            my_server:call(Pid, {unicast, #message{timestamp=io_lib:format("[~2..0b:~2..0b:~2..0b]", tuple_to_list(time())), name=Name, text=MainText}, ReceiverName});
        "/q"++ _ ->
            exit(normal);
        _ ->
            my_server:call(Pid, {broadcast, #message{timestamp=io_lib:format("[~2..0b:~2..0b:~2..0b]", tuple_to_list(time())), name=Name, text=Text}})
    end,
    
    send_to_server(Name, Pid).

start_link() -> my_server:start_link(?MODULE, []).

%%% Server functions
init([]) -> #state{msglist=[], scribers=[], clientinfo=dict:new()}. %% no treatment of info here!

send_to_clients(_, []) ->
    ok;
send_to_clients(Message, [Scriber | ScriberList]) ->
    my_server:reply(Scriber, Message),
    send_to_clients(Message, ScriberList).

handle_call({unicast, Message, ReceiverName}, From, S = #state{clientinfo=Dict}) ->
    my_server:reply(From, "Sent"),
    Receiver = dict:fetch(ReceiverName, Dict),
    send_to_clients(Message, [Receiver]),
    S;

handle_call({broadcast, Message}, From, S = #state{msglist=MList, scribers=ScriberList}) ->
    NewMList = lists:reverse([ Message |lists:reverse(MList)]),
    my_server:reply(From, "Sent"),
    send_to_clients(Message, ScriberList),
    S#state{msglist=NewMList};

handle_call({subscribe, Name}, From, S = #state{msglist=MList, scribers=ScriberList, clientinfo=Dict}) ->
    NewScriberList = [From | ScriberList],
    NewDict = dict:store(Name, From, Dict),
    my_server:reply(From, lists:reverse(lists:sublist(lists:reverse(MList), 3))),
    S#state{scribers=NewScriberList, clientinfo=NewDict};

handle_call(terminate, From, _) ->
    my_server:reply(From, ok),
    terminate().

% handle_cast({return, Cat = #cat{}}, Cats) ->
    % [Cat|Cats].

%%% Private functions
terminate() ->
    exit(normal).
