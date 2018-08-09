-module(echat).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("headers.hrl").

-record(state, {msglist, scribers, clientinfo, maxclients=2}).

start_link() ->
    gen_server:start_link({local, chatsv}, ?MODULE, [], []).

%%% Server functions
init([]) -> {ok,#state{msglist=[], scribers=[], clientinfo=dict:new()}}. %% no treatment of info here!

send_to_clients(_, []) ->
    ok;
send_to_clients(Message, [Scriber | ScriberList]) ->
    gen_server:reply(Scriber, Message),
    send_to_clients(Message, ScriberList).

handle_call({unicast, Message, ReceiverName}, _, S = #state{clientinfo=Dict}) ->
    Receiver = dict:fetch(ReceiverName, Dict),
    send_to_clients(Message, [Receiver]),
    {reply, "Sent", S};

handle_call({broadcast, Message}, _, S = #state{msglist=MList, scribers=ScriberList}) ->
    NewMList = lists:reverse([ Message |lists:reverse(MList)]),
    send_to_clients(Message, ScriberList),
    {reply, "Sent", S#state{msglist=NewMList}};

handle_call({getclist}, _, S = #state{clientinfo=Dict}) ->
    {reply, dict:fetch_keys(Dict), S};

handle_call({disconnect, Name}, _, S = #state{scribers=ScriberList, clientinfo=Dict}) ->
    Receiver = dict:fetch(Name, Dict),
    NewScriberList = lists:delete(Receiver, ScriberList),
    NewDict = dict:erase(Name, Dict),
    send_to_clients(quit, [Receiver]),
    {reply, "Sent", S#state{scribers=NewScriberList, clientinfo=NewDict}};

handle_call({subscribe, Name}, From, S = #state{msglist=MList, scribers=ScriberList, clientinfo=Dict}) ->
    if
        length(ScriberList) == S#state.maxclients ->
                {reply, {error, "Error: Room is full!"} , S};
            
        true ->
                NewScriberList = [From | ScriberList],
                NewDict = dict:store(Name, From, Dict),
                {reply, {ok, lists:reverse(lists:sublist(lists:reverse(MList), 3))}, S#state{scribers=NewScriberList, clientinfo=NewDict}}
    end;
    

handle_call(terminate, From, _) ->
    gen_server:reply(From, ok),
    terminate().

handle_cast({return}, S) ->
    S.

%%% Private functions
terminate() ->
    exit(normal).
