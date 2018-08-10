-module(echat).

-behaviour(gen_server).

-export([start/0, start_link/0, start_mnesia/0]).

-export([handle_call/3, handle_cast/2, init/1]).

-include("headers.hrl").

-record(state,
	{msglist, scribers, clientinfo, maxclients = 200}).

start_mnesia() ->
    mnesia:start(),
    mnesia:wait_for_tables([message, scriber, clientinfo],
			   20000).

start() -> start_mnesia(), start_link().

start_link() ->
    gen_server:start_link({local, chatsv}, ?MODULE, [], []).

%%% Utilities

remove_first_column(RecList) ->
    [list_to_tuple(tl(tuple_to_list(Rec)))
     || Rec <- RecList].

%%% Server functions
init([]) ->
    {atomic, MList} = message:get_all(),
    {atomic, ScriberList} = scriber:get_all(),
    {atomic, ClientInfoList} = clientinfo:get_all(),
    ClientInfoDict =
	dict:from_list(remove_first_column(ClientInfoList)),
    {ok,
     #state{msglist = MList,
	    scribers = remove_first_column(ScriberList),
	    clientinfo =
		ClientInfoDict}}. %% no treatment of info here!

send_to_clients(_, []) -> ok;
send_to_clients(Message, [Scriber | ScriberList]) ->
    gen_server:reply(Scriber, Message),
    send_to_clients(Message, ScriberList).

handle_call({unicast, Message, ReceiverName}, _,
	    S = #state{msglist = MList, clientinfo = Dict}) ->
    Receiver = dict:fetch(ReceiverName, Dict),
    NewMList = lists:reverse([Message
			      | lists:reverse(MList)]),
    message:create_or_update(tmp, Message, []),
    send_to_clients(Message, [Receiver]),
    {reply, "Sent", S#state{msglist = NewMList}};
handle_call({broadcast, Message}, _,
	    S = #state{msglist = MList, scribers = ScriberList}) ->
    NewMList = lists:reverse([Message
			      | lists:reverse(MList)]),
    message:create_or_update(tmp, Message, []),
    send_to_clients(Message, ScriberList),
    {reply, "Sent", S#state{msglist = NewMList}};
handle_call({getclist}, _,
	    S = #state{clientinfo = Dict}) ->
    {reply, dict:fetch_keys(Dict), S};
handle_call({disconnect, Name}, _,
	    S = #state{scribers = ScriberList,
		       clientinfo = Dict}) ->
    Receiver = dict:fetch(Name, Dict),
    NewScriberList = lists:delete(Receiver, ScriberList),
    NewDict = dict:erase(Name, Dict),
    send_to_clients(quit, [Receiver]),
    {reply, "Sent",
     S#state{scribers = NewScriberList,
	     clientinfo = NewDict}};
handle_call({subscribe, Name}, From,
	    S = #state{msglist = MList, scribers = ScriberList,
		       clientinfo = Dict}) ->
    if length(ScriberList) == S#state.maxclients ->
	   {reply, {error, "Error: Room is full!"}, S};
       true ->
	   NewScriberList = [From | ScriberList],
	   {Pid, Ref} = From,
	   scriber:create_or_update(tmp,
				    #scriber{pid = Pid, ref = Ref}, []),
	   NewDict = dict:store(Name, From, Dict),
	   clientinfo:create_or_update(tmp,
				       #clientinfo{name = Name, id = From}, []),
	   %io:format("Here is the state ~p~n", [S]),
	   %LastMList = lists:reverse(lists:sublist(lists:reverse(MList), 3))},
	   LastMList = retrieve_last_n_msgs(lists:reverse(MList),
					    NewDict, From, [], 0),
	%    io:format("Here is last mlist~p~n", [LastMList]),
	   {reply, {ok, LastMList},
	    S#state{scribers = NewScriberList,
		    clientinfo = NewDict}}
    end;
handle_call(terminate, From, _) ->
    gen_server:reply(From, ok), terminate().

handle_cast({return}, S) -> S.

%%% Private functions
retrieve_last_n_msgs([], _Dict, _Receiver, AccMList,
		     _) ->
    AccMList;
retrieve_last_n_msgs(_MList, _Dict, _Receiver, AccMList,
		     3) ->
    AccMList;
retrieve_last_n_msgs(MList, Dict, Receiver, AccMList,
		     Count) ->
    [Message | Rest] = MList,
    % io:format("MSG~p,Acc List~p~n", [Message,AccMList]),
    case Message#message.receiver of
      broadcast ->
        %   io:format("Found a broadcast ~p , ~p", [Message,AccMList]),
	  retrieve_last_n_msgs(Rest, Dict, Receiver,
			       [Message | AccMList], Count + 1);
      _ ->
	  case dict:fetch(Message#message.receiver, Dict) of
	    R when R == Receiver ->
            % io:format("Matched Receiver"),
		retrieve_last_n_msgs(Rest, Dict, Receiver,
				     [Message | AccMList], Count + 1);
	    _ ->
		retrieve_last_n_msgs(Rest, Dict, Receiver,
				     AccMList, Count)
	  end
    end.

terminate() -> exit(normal).
