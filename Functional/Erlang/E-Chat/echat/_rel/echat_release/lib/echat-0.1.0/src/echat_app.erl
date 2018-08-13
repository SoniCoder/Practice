-module(echat_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-spec start(any(), any()) -> pid().
start(_Type, _Args) -> echat_sup:start_link().

-spec start() -> any().
start() -> application:ensure_all_started(echat).

-spec stop(any()) -> ok.
stop(_State) -> ok.
