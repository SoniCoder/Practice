-module(test_mnesia).
-record(test, {test}).

-export([doonce/0, start/0]).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([test], 20000).


doonce() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(test, [{attributes, record_info(fields, test)}]),
	mnesia:stop().
