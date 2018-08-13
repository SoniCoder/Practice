-module(client_api).

-export([close_server/0, con/2]).

-include("headers.hrl").

%%% Client Utility Functions

-spec current_timestamp() -> any().

current_timestamp() ->
    io_lib:format("[~2..0b:~2..0b:~2..0b]",
		  tuple_to_list(time())).

-spec create_message(string(), string()) -> message().

create_message(Message, Sender) ->
    create_message(Message, Sender, broadcast, false).

-spec create_message(string(), string(), string(),
		     boolean()) -> message().

create_message(Message, Sender, ReceiverName,
	       Private) ->
    #message{time = time(), timestamp = current_timestamp(),
	     name = Sender, text = Message, receiver = ReceiverName,
	     private = Private}.

-spec printStringList([string()]) -> any().

printStringList([]) -> ok;
printStringList([H | T]) ->
    printMessage(H), printStringList(T).

-spec printList([string()]) -> any().

printList([]) -> ok;
printList([H | T]) ->
    io:format("~p~n", [H]), printList(T).

printMessage(M) ->
    % io:format("This is M~p~n", [M]),
    if M#message.private == false ->
	   io:format("~-12s ~-10s : ~s",
		     [M#message.timestamp, M#message.name, M#message.text]);
       M#message.private == true ->
	   io:format("~-12s ~-10s : ~s",
		     [M#message.timestamp, "[" ++ M#message.name ++ "]",
		      M#message.text])
    end.

keep_receiving() ->
    receive
      {_, quit} -> ok;
      {_, M} -> printMessage(M), keep_receiving()
      after infinity -> erlang:error(timeout)
    end.

send_to_server(Name, SV_NAME) ->
    Text = io:get_line("enter message> "),
    {ok, RE_PRV} = re:compile("/w (\\w+) (.+\\n)"),
    case Text of
      "/l" ++ _ ->
	  ClientList = gen_server:call(whereis(SV_NAME),
				       {getclist}),
	  printList(ClientList);
      "/w" ++ _ ->
	  {match, [ReceiverName, MainText]} = re:run(Text, RE_PRV,
						     [{capture, all_but_first,
						       list}]),
	  gen_server:call(whereis(SV_NAME),
			  {unicast,
			   create_message(MainText, Name, ReceiverName, true),
			   ReceiverName});
      "/q" ++ _ ->
	  gen_server:call(whereis(SV_NAME), {disconnect, Name}),
	  gen_server:call(whereis(SV_NAME),
			  {broadcast,
			   create_message(lists:concat([Name,
							" has left the server.\n"]),
					  "*Server*")}),
	  exit(normal);
      X when is_list(X) ->
	  gen_server:call(whereis(SV_NAME),
			  {broadcast, create_message(Text, Name)});
      _ -> ok
    end,
    send_to_server(Name, SV_NAME).

%%% Client API

con(out, Name) ->
    Pid = whereis(chatsv),
    M = gen_server:call(Pid, {subscribe, Name}),
    case M of
      {ok, MList} -> printStringList(MList), keep_receiving();
      {error, ErrMsg} -> ErrMsg
    end;
con(in, Name) ->
    Pid = whereis(chatsv),
    io:format("Welcome to E-Chat!~n~n"),
    gen_server:call(Pid,
		    {broadcast,
		     create_message(lists:concat([Name,
						  " has entered the server.\n"]),
				    "*Server*")}),
    send_to_server(Name, chatsv),
    ok.

close_server() ->
    gen_server:call(whereis(chatsv), {terminate}).
