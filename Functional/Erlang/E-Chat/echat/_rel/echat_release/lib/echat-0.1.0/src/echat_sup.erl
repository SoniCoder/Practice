-module(echat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-spec start_link() -> any().
start_link() ->
    supervisor:start_link(?MODULE, []).


-spec init(any()) -> any().
init(_Args) ->
    {ok,
     {{one_for_one, 10, 60},
      [{chatserver, {echat, start, []},
	permanent, 2000, worker, [echat]}
       ]}}.