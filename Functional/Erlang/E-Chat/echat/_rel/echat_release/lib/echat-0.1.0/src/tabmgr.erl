-module(tabmgr).

-export([start/0]).

-include("headers.hrl").

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(message,
			[{attributes, record_info(fields, message)},
			 {disc_copies, [node()]}, {type, ordered_set}]),
    mnesia:create_table(scriber,
			[{attributes, record_info(fields, scriber)},
			 {disc_copies, [node()]}]),
    mnesia:create_table(clientinfo,
			[{attributes, record_info(fields, clientinfo)},
			 {disc_copies, [node()]}]),
    mnesia:stop().
