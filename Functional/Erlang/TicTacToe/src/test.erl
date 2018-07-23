-module(test).

-import(ttt,
	[accept_connect/1, cancel/1, connect/2, decide_move/2,
	 move/3, printBoard/1, start_link/1]).

-export([acc/1, close/1, con/1, illmove/1, move/1, print/1, start/1]).

acc(C) -> accept_connect(whereis(C)).

close(C) -> cancel(whereis(C)).

con(C) ->
    case C of
      c1 -> connect(whereis(c1), whereis(c2));
      c2 -> connect(whereis(c2), whereis(c1))
    end.

illmove(C) ->
    case C of
      c1 -> decide_move(whereis(c1), whereis(c2));
      c2 -> decide_move(whereis(c2), whereis(c1))
    end.

move(X) -> move(whereis(c1), whereis(c2), X).

print(C) -> printBoard(whereis(C)).

start(C) ->
    case C of
      c1 -> {ok, OwnPid} = start_link("c1");
      c2 -> {ok, OwnPid} = start_link("c2")
    end,
    register(C, OwnPid),
    {ok, OwnPid}.
