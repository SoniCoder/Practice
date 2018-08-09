-module(baseinfo).

-include("../headers.hrl").

-type defaultvalue() :: term().

-export([
    create/4,
    create_all/2,
    create_all/1,
    delete/2,
    delete_all/1,
    bulk_delete/1,
    get_all/1,
    get_all/2,
    get_by_id/2,
    get_by_id/3,
    update/5,
    update/4,
    update/3,
    update_create_terms/4,
    search_by/3,
    get_column_by_id/3,
    get_column_by_id/4,
    get_id_by_index/3,
    get_record_by_index/3,
    delete_record_by_column/3
]).

-callback create_or_update(key(), value(), metadata()) -> {ok, Key :: key()} | {error, term()}.


-callback delete_all() -> ok|{error, term()}.


-callback delete(key()) -> ok|{error, term()}.


-callback update(key(), {module(), atom(), list(term())}, list({no_create, true|false} | {pass_metadata, true|false}), list(term())) -> ok | {ok, key()} | any().

-callback get_all() -> {ok, list(output())} | {error, term()}.


-callback get_by_id(key()) -> {ok, output()} | {error, term()}.


-callback get_by_id(key(), db_columnlist()|record) -> {ok, list()} | {error, term()}.


-callback search_by(db_whereclauselist(), db_columnlist()|record|key) -> {ok, list()} |{error, term()}.

-callback get_column_by_id(key(), db_columnname()) -> {ok, term()}|{error, term()}.

-callback get_column_by_id(key(), db_columnname(), defaultvalue()) -> term().

-callback update_columns_by_id(key(), list({atom(), term()})) -> {ok, key()}|{error, term()}.

-spec create(bucket(), key(), value(), metadata()) -> {ok, key()} | {error, term()}.
create(Bucket, Key, Value, Metadata) ->
    db_functions:db_create_or_update_term(Bucket, Key, Value, Metadata).

-spec create_all(bucket(), list({key(), value(), metadata()})) -> {ok, list({add, key(), value()})} | {error, term()}.
create_all(Bucket, Items) ->
    db_functions:db_store_terms(Bucket, Items).

-spec create_all(list({bucket(), key(), value(), metadata()})) -> {ok, list({add, key(), value()})} | {error, term()}.
create_all(Items) ->
    db_functions:db_store_terms(Items).

-spec delete_all(bucket()) -> ok | {error, term()}.
delete_all(Bucket) ->
    db_functions:delete_all(Bucket).

-spec delete(bucket(), key()) -> ok | {error, term()}.
delete(Bucket, Key) ->
    db_functions:db_delete_key(Bucket, Key).

-spec bulk_delete(list({bucket(), list(key())})) -> ok | {error, term()}.
bulk_delete(Items) ->
    db_functions:db_bulk_delete(Items).

-spec update_create_terms(
        bucket(),
        {atom(), atom(), db_columnname(), list() | term()},
        list({bucket(), key(), value(), metadata()}),
        atom()
) -> {ok, list()} | {error, term()}.
update_create_terms(Bucket, UpdateArgs, OtherOperationList, {UpdateModule, UpdateFunction}) ->
    db_functions:db_update_create_terms(Bucket, UpdateArgs, OtherOperationList, {UpdateModule, UpdateFunction}).

-spec update(
        bucket(),
        {atom(), atom(), db_columnname(), list() | term()},
        atom()
) -> {ok, list()} | {error, term()}.
update(Bucket, UpdateArgs, {UpdateModule, UpdateFunction}) ->
    db_functions:db_update_create_terms(Bucket, UpdateArgs, [], {UpdateModule, UpdateFunction}).

-spec update(
        bucket(),
        {atom() | integer(), key()} | list(key()) | key(),
        {module(), atom(), list(term())} | value(),
        {module(), atom(), list(term()) | metadata()}
) -> {ok, list() | key()} | {error, term()}.
update(Bucket, InputArgs, {CheckModule, CheckFunction, CheckArgs}, {UpdateModule, UpdateFunction, UpdateArgs}) ->
    db_functions:db_update_terms(Bucket, InputArgs, {CheckModule, CheckFunction, CheckArgs}, {UpdateModule, UpdateFunction, UpdateArgs}).

-spec update(bucket(), key(), {module(), atom(), list(term())}, list({no_create, true|false} | {pass_metadata, true|false}), list(term()))
            ->
            ok | {ok, key()} | any().
update(Bucket, Key, {Module, Function, Args}, Options, _UpdateOptions) ->
    db_functions:update(Bucket, Key, {Module, Function, Args}, Options, _UpdateOptions).

-spec get_all(bucket()) -> {ok, list()} | {error, term()}.
get_all(Bucket) ->
    db_functions:get_all_data(Bucket, record).

-spec get_all(bucket(), list()|key) -> {ok, list()}|{error, term()}.
get_all(Bucket, ColumnList) ->
    db_functions:get_all_data(Bucket, ColumnList).


-spec get_by_id(bucket(), key()) -> {ok, output()}|{error, term()}.
get_by_id(Bucket, Key) ->
    case get_by_id(Bucket, Key, record) of
        {ok, Results} ->
            case Results of
                [] ->
                    {error, notfound};
                _Else ->
                    {ok, hd(Results)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_by_id(bucket(), key(), db_columnlist()|record) -> {ok, list()}|{error, term()}.
get_by_id(Bucket, Key, ColumnList) ->
    db_functions:get_data_by_key(Bucket, Key, ColumnList).


-spec get_column_by_id(bucket(), key(), db_columnname()) -> {ok, term()}|{error, term()}.
get_column_by_id(Bucket, Key, ColumnName) ->
    db_functions:get_column_by_key(Bucket, Key, ColumnName).


-spec get_column_by_id(bucket(), key(), db_columnname(), defaultvalue()) -> term()|{error|term()}.
get_column_by_id(Bucket, Key, ColumnName, DefaultValue) ->
    case db_functions:get_column_by_key(Bucket, Key, ColumnName) of
        {error, notfound} ->
            DefaultValue;
        {error, Reason} ->
            {error, Reason};
        {ok, Result} ->
            Result
    end.

-spec get_id_by_index(bucket(), db_columnname(), term()) -> {ok, list(term())} | {error, timeout}.
get_id_by_index(Bucket, ColumnName, Value) ->
    db_functions:db_get_term_by_index(Bucket, ColumnName, Value, keys).

-spec get_record_by_index(bucket(), db_columnname(), term()) -> {ok, list(term())} | {error, timeout}.
get_record_by_index(Bucket, ColumnName, Value) ->
    db_functions:db_get_term_by_index(Bucket, ColumnName, Value, records).

-spec search_by(bucket(), db_whereclauselist(), db_columnlist()|record|key) -> {ok, list()}|{error|term()}.
search_by(Bucket, WhereClauseList, ColumnList) ->
    db_functions:search_by(Bucket, WhereClauseList, ColumnList).

-spec delete_record_by_column(bucket(), db_columnname(), term()) -> ok | {error, term()}.
delete_record_by_column(Bucket, ColumnName, Value) ->
    db_functions:delete_record_by_column(Bucket, ColumnName, Value).

%%-spec update_all(bucket(), db_columnname(), term()) -> {ok, }
%%update_all() ->
%%    {ok, Keys} = butlerinfo:get_all(key),
%%    lists:foreach(
%%        fun(Key) ->
%%            {ok, _Key} = butlerinfo:update_columns_by_id(Key, [{status, dead}])
%%        end,
%%        Keys),
%%    ok.

