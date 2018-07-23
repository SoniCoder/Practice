-module(ttt_sup).

-behaviour(supervisor).

-export([start_link/1, start/1]).

-export([init/1]).

start(C) -> start_link(C).

start_link(C) ->
    supervisor:start_link({local, list_to_atom("sup" ++ atom_to_list(C))}, ?MODULE, C).

init(c1) -> init({c1, one_for_one, 3, 30});
init(c2) -> init({c2, one_for_one, 2, 60});
init({C, RestartStrategy, MaxRestart, MaxTime}) ->
    {ok,
     {{RestartStrategy, MaxRestart, MaxTime},
      [{C, {test, start, [C]},
	permanent, 2000, worker, [ttt]}
       ]}}.
