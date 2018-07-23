-module(ttt).

-behaviour(gen_fsm).

-record(state,
	{name = "", other, board = array:new(9, {default, "_"}),
	 monitor, next_char = "X", from}).

%% public API
-export([accept_connect/1, cancel/1, connect/2,
	 decide_move/2, move/3, printBoard/1, start/1,
	 start_link/1]).

%% gen_fsm callbacks
-export([code_change/4, handle_event/3, handle_info/3,
	 handle_sync_event/4, idle/2, idle/3, idle_wait/2,
	 idle_wait/3, init/1, negotiate/2, readytomove/2,
	 terminate/3, waitformove/2]).

% custom state names

%%% Utility Functions

%%% PUBLIC API
start(Name) -> gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

connect(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid},
			    30000).

%% Accept someone's trade offer.
accept_connect(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

decide_move(OwnPid, OtherPid) ->
    gen_fsm:send_event(OwnPid, {move}),
    gen_fsm:send_event(OtherPid, {waitmove}).

getBoardString(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, printBoard).

printBoard({board, BoardString}) ->
    io:format("~s~n", [BoardString]);
printBoard(OwnPid) ->
    printBoard({board, getBoardString(OwnPid)}).

move(OwnPid, OtherPid, X) ->
    gen_fsm:send_event(OwnPid, {moveMade, X}),
    gen_fsm:send_event(OtherPid, {moveMade, X}).

cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

% Ask the other FSM's Pid for a trade session
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid,
		       {accept_negotiate, OwnPid}).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).

%% Server Side Utility Functions

checkBoard(Board) ->
    L1 = array:get(0, Board),
    L2 = array:get(1, Board),
    L3 = array:get(2, Board),
    L4 = array:get(3, Board),
    L5 = array:get(4, Board),
    L6 = array:get(5, Board),
    L7 = array:get(6, Board),
    L8 = array:get(7, Board),
    L9 = array:get(8, Board),
    if L1 == L2, L2 == L3, L1 /= "_" -> Winner = L1;
       L4 == L5, L5 == L6, L4 /= "_" -> Winner = L4;
       L7 == L8, L8 == L9, L7 /= "_" -> Winner = L7;
       L1 == L4, L4 == L7, L1 /= "_" -> Winner = L1;
       L2 == L5, L5 == L8, L2 /= "_" -> Winner = L2;
       L3 == L6, L6 == L9, L3 /= "_" -> Winner = L3;
       L1 == L5, L5 == L9, L1 /= "_" -> Winner = L1;
       L3 == L5, L5 == L7, L3 /= "_" -> Winner = L3;
       true -> Winner = none
    end,
    if Winner == none ->
	   case lists:member("_", array:to_list(Board)) of
	     true -> none;
	     false -> io:format("Draw!~n"), draw
	   end;
       true -> {gameover, Winner}, io:format("GameOver!~n")
    end.

%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(#state{name = N}, Str, Args) ->
    io:format("~s: " ++ Str ++ "~n", [N | Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in "
	      "state ~p~n",
	      [self(), Msg, State]).

createBoardString(Board) ->
    io_lib:format("\n~s ~s ~s \n~s ~s ~s \n~s ~s ~s \n ",
		  array:to_list(Board)).

init(Name) -> {ok, idle, #state{name = Name}}.

idle({ask_negotiate, OtherPid}, S = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a game connection", [OtherPid]),
    {next_state, idle_wait,
     S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle), {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, S = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a game connection",
	   [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait,
     S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle), {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid},
	  S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "connected", []),
    {next_state, negotiate, S};
%% The other side has accepted our offer. Move to negotiate state
idle_wait({accept_negotiate, OtherPid},
	  S = #state{other = OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "connected", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From,
	  S = #state{other = OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "connected", []),
    {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

negotiate({move}, S = #state{board = Board}) ->
    notice(S, "Your turn to move!", []),
    printBoard({board, createBoardString(Board)}),
    {next_state, readytomove, S#state{}};
negotiate({waitmove}, S = #state{board = Board}) ->
    notice(S, "Opponent is first to move!", []),
    printBoard({board, createBoardString(Board)}),
    {next_state, waitformove, S#state{}};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.

readytomove({moveMade, X},
	    S = #state{board = Board, next_char = NextChar}) ->
    NewBoard = array:set(X - 1, NextChar, Board),
    case NextChar of
      "X" -> NewNextChar = "O";
      "O" -> NewNextChar = "X"
    end,
    printBoard({board, createBoardString(NewBoard)}),
    checkBoard(NewBoard),
    {next_state, waitformove,
     S#state{board = NewBoard, next_char = NewNextChar}};
readytomove(Event, Data) ->
    unexpected(Event, readytomove),
    {next_state, readytomove, Data}.

waitformove({moveMade, X},
	    S = #state{board = Board, next_char = NextChar}) ->
    NewBoard = array:set(X - 1, NextChar, Board),
    case NextChar of
      "X" -> NewNextChar = "O";
      "O" -> NewNextChar = "X"
    end,
    printBoard({board, createBoardString(NewBoard)}),
    checkBoard(NewBoard),
    {next_state, readytomove,
     S#state{board = NewBoard, next_char = NewNextChar}};
waitformove(Event, Data) ->
    unexpected(Event, waitformove),
    {next_state, waitformove, Data}.

handle_event(cancel, _StateName, S = #state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName,
		  S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, S};
handle_sync_event(printBoard, _From, StateName,
		  S = #state{board = Board}) ->
    {reply, createBoardString(Board), StateName, S};
%% Note: DO NOT reply to unexpected calls. Let the call-maker crash!
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _,
	    S = #state{other = Pid, monitor = Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%% Transaction completed.
terminate(normal, ready, S = #state{}) ->
    notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) -> ok.
