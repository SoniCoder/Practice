-module(tabmgr).

-export([start/0]).

-include("headers.hrl").

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(message,
            [{attributes, record_info(fields, message)},{disc_copies, [node()]}]),
    mnesia:stop().
