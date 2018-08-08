%% ----------------------------------------------------------------------
%% @author Srijan Choudhary <srijan4@gmail.com>
%% @doc Functions for database stuff.
%% @end
%% ----------------------------------------------------------------------

-module(db_functions).

%% For database stuff

-type key() :: term().
-type bucket() :: atom().
-type value() :: term().
-type metadata() :: list().
-type db_operator()                 :: atom().
-type db_whereclauselist()          :: list({atom(),db_operator(),term()}).
-type db_columnlist()               :: list(atom()).
-type db_columnname()               :: atom().

-define(TIMEOUT_VAL, application:get_env(butler_database, db_timeout, 1500)).

%% API
-export([
    db_store_term_fun/4,
    db_store_term/4,
    db_store_terms/2,
    db_store_terms/1,
    remove_2i_entries_fun/2,
    db_get_terms/2,
    db_get_term/2,
    db_get_term_by_index/4,
    db_get_term_multiple_2i/3,
    db_get_term_2i/3,
    db_get_term_2i/4,
    db_get_term_2i/5,
    db_create_or_update_term/4,
    db_create_term/5,
    db_update_term/5,
    db_update_terms/4,
    db_update_create_terms/4,
    db_list_keys/1,
    db_delete_key/2,
    db_delete_keys/2,
    db_bulk_delete/1,
    db_delete_record/2,
    db_exec_qlc/1,
    db_query_pgsql/1,
    db_query_pgsql_ext/2,
    db_save_riemann_event/2,
    db_save_riemann_metric/3,
    db_get_terms_in_range/4,
    db_delete_table/1,
    db_dirty_write/2,
    db_dirty_read/2,
    get_all_data/2,
    get_all_data_from_db/2,
    get_data_by_key/3,
    get_column_by_key/3,
    search_by/3,
    ppstaskrec_meta_search_by/2,
    delete_all/1,
    update/5,
    get_index/2,
    clear_cache/2,
    clear_cache/3,
    flush_cache/2,
    flush_cache/3,
    delete_record_by_column/3
]).

-include_lib("stdlib/include/qlc.hrl").

%% @private
%% @doc Take a pooler member, retry if not successful.
-spec take_pool_member(PoolName :: atom()) -> pid().
take_pool_member(PoolName) ->
    poolboy:checkout(PoolName).

-spec return_pool_member(atom(), pid()) -> ok.
return_pool_member(PoolName, Worker) ->
    poolboy:checkin(PoolName, Worker).

-spec db_store_terms(bucket(), list({key(), value(), metadata()})) -> {ok, list({add, key(), value()})} | {error, term()}.
db_store_terms(Bucket, StoreList) ->
    case mnesia:transaction(
           fun() ->
                lists:map(
                    fun(StoreTerm) ->
                        {Key, Value, Metadata} = StoreTerm,
                        CacheExclusion =
                            case db_functions:db_get_term(Bucket, Key) of
                                {error, notfound} ->
                                    [];
                                _ ->
                                    [{get_all_data, [key]}]
                            end,
                        case db_functions:db_store_term_fun(Bucket, Key, Value, Metadata) of
                            {ok, Key} ->
                                db_functions:flush_cache(Bucket, Key, CacheExclusion),
                                {add, Key, Value};
                            {error, Reason}->
                                {error, Reason}
                        end
                    end,
                    StoreList)
           end) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec db_store_terms(list({bucket(), key(), value(), metadata()})) -> {ok, list({add, key(), value()})} | {error, term()}.
db_store_terms(StoreList) ->
    case mnesia:transaction(
           fun() ->
                lists:map(
                    fun(StoreTerm) ->
                        {Bucket, Key, Value, Metadata} = StoreTerm,
                        CacheExclusion =
                            case db_functions:db_get_term(Bucket, Key) of
                                {error, notfound} ->
                                    [];
                                _ ->
                                    [{get_all_data, [key]}]
                            end,
                        case db_functions:db_store_term_fun(Bucket, Key, Value, Metadata) of
                            {ok, Key} ->
                                db_functions:flush_cache(Bucket, Key, CacheExclusion),
                                {add, Key, Value};
                            {error, Reason}->
                                {error, Reason}
                        end
                    end,
                    StoreList)
           end) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Stores a term in the database.
-spec db_store_term(bucket(), key(), value(), metadata()) ->
                           {ok, Key :: key()} | {error, term()}.
db_store_term(Bucket, Key, Value, Metadata) ->
    case mnesia:transaction(
           fun() ->
                   db_functions:db_store_term_fun(Bucket, Key, Value, Metadata)
           end) of
        {atomic, _Result} ->
            {ok, Key};
        {aborted, Reason} ->
            {error, Reason}
    end.

db_dirty_write(Bucket, Value) ->
    mnesia:dirty_write(Bucket, Value).

-spec db_dirty_read(bucket(), value()) -> {ok, term()} | {error, term()}.
db_dirty_read(Bucket, Value) ->
    case mnesia:dirty_read(Bucket, Value) of
      {aborted, Reason} ->
        {error, Reason};
      ResultList ->
        case ResultList of
          [] ->
            {error, notfound};
          [H | _T] ->
            {ok, H}
        end
        end.

-spec db_store_term_fun(bucket(), key(), value(), metadata()) ->
                           {ok, Key :: key()} | {error, term()}.

db_store_term_fun(ppstaskrec = Bucket, Key, Value, Metadata) ->
    ok = mnesia:write(Bucket, Value, write),
    remove_2i_entries_fun_dirty(Bucket, Key),
    lists:foreach(
      fun({Tab, SKey}) ->
              mnesia:write(Tab, {Tab, SKey, Key}, write)
      end,
      Metadata
     ),
    {ok, Key};
db_store_term_fun(Bucket, Key, Value, Metadata) ->
    ok = mnesia:write(Bucket, Value, write),
    db_functions:remove_2i_entries_fun(Bucket, Key),
    lists:foreach(
      fun({Tab, SKey}) ->
              mnesia:write(Tab, {Tab, SKey, Key}, write)
      end,
      Metadata
     ),
    {ok, Key}.

-spec remove_2i_entries_fun(bucket(), key()) -> ok.
remove_2i_entries_fun(Bucket, Key) ->
    %% TODO: Optimize
    SIndexTables =
        [element(1, STableDetails) ||
            STableDetails <-
                proplists:get_value(sindex, proplists:get_value(Bucket, models:all(), []), []) ],
    lists:foreach(
      fun(IndexTable) ->
              SRecords =
                  qlc:e(
                    qlc:q(
                      [ Rec ||
                          Rec <- mnesia:table(IndexTable),
                          element(3, Rec) == Key
                      ])),
              [ mnesia:delete_object(SRecord) || SRecord <- SRecords ]
      end,
      SIndexTables
     ).

remove_2i_entries_fun_dirty(Bucket, Key) ->
    SIndexTables =
        [element(1, STableDetails) ||
            STableDetails <-
                proplists:get_value(sindex, proplists:get_value(Bucket, models:all(), []), []) ],
    lists:foreach(
        fun(IndexTable) ->
            SRecords = mnesia:dirty_select(IndexTable,
                        [{{'_','_',Key}, [], ['$_']}]
                      ),
            [mnesia:delete_object(SRecord) || SRecord <- SRecords ]
        end,
        SIndexTables).

%% @doc Gets a term from the database. Assumes that the table is of `set' type.
-spec db_get_term(bucket(), key()) ->
                         {ok, value()} | {error, notfound}.
db_get_term(Bucket, Key) ->
    case db_functions:db_get_terms(Bucket, Key) of
        {ok, []} ->
            {error, notfound};
        {ok, RecordList} ->
            {ok, hd(RecordList)}
    end.

%% @doc Returns list of records from a table with specified key
%% If table type is dict, one key has one record
%% If table type is bag, one key can have many records
-spec db_get_terms(bucket(), key()) -> {ok, list()} | {error, term()}.
db_get_terms(Bucket, Key) ->
    case mnesia:transaction(
           fun() ->
                   mnesia:read(Bucket, Key, read)
           end
          ) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec db_get_term_by_index(bucket(), db_columnname(), term(), keys | records) -> {ok, list(term())} | {error, timeout}.
db_get_term_by_index(Bucket, ColumnName, Value, Option) ->
    AttrList = get_tableinfo(Bucket, attributes),
    Index = get_index(ColumnName, AttrList) + 1,
    db_get_term_2i(Bucket, Index, Value, Option).

-spec db_get_terms_in_range(bucket(), integer(), {key(), key()}, keys | records) -> {ok, list()} | {error, term()}.
db_get_terms_in_range(Bucket, Index, {Min, Max}, Option) ->
    Fun =
        case Option of
            keys ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             element(2, Record) ||
                                Record <- mnesia:table(Bucket),
                                element(Index, Record) >= Min,
                                element(Index, Record) =< Max
                            ]))
                end;
            records ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             Record ||
                                Record <- mnesia:table(Bucket),
                                element(Index, Record) >= Min,
                                element(Index, Record) =< Max
                            ]))
                end
        end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec db_get_term_multiple_2i(bucket(), list({integer(), key()}), keys | records) ->
                                     {ok, list(term())} | {error, timeout}.
db_get_term_multiple_2i(Bucket, IndexList, Option) ->
    Fun =
        fun() ->
                qlc:e(
                  lists:foldl(
                    fun({Index, Key}, AccIn) ->
                            case is_atom(Index) of
                                true ->
                                    qlc:q(
                                    [
                                     Record ||
                                        Record <- AccIn,
                                        SRecord <- mnesia:table(Index),
                                        element(2, SRecord) == Key,
                                        element(2, Record) == element(3, SRecord)
                                    ]);
                                false ->
                                    qlc:q(
                                      [
                                       Record ||
                                          Record <- AccIn,
                                          element(Index, Record) == Key
                                      ])
                            end
                    end,
                    mnesia:table(Bucket),
                    IndexList
                   ),
                  cache_all
                 )
        end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            case Option of
                keys ->
                    {ok, [element(2, Rec) || Rec <- Result]};
                records ->
                    {ok, Result}
            end;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec db_get_term_2i(bucket(), atom() | integer(), key()) ->
                            {ok, list(term())} | {error, timeout}.
db_get_term_2i(Bucket, Index, Key) ->
    db_get_term_2i(Bucket, Index, Key, keys).

%% @doc Returns a list of keys/records matching the secondary index.
%% If Index is integer(), it is a secondary index on the same table
%% If Index is atom(), it is an sindex (i.e, using another table)
-spec db_get_term_2i(bucket(), atom() | integer(), key(), keys | records) ->
                            {ok, list(term())} | {error, timeout}.

db_get_term_2i(Bucket, Index, Key, Option) when is_integer(Index) ->
    Fun =
        case Option of
            keys ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             element(2, Record) ||
                                Record <- mnesia:table(Bucket),
                                element(Index, Record) == Key
                            ]))
                end;
            records ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             Record ||
                                Record <- mnesia:table(Bucket),
                                element(Index, Record) == Key
                            ]))
                end
        end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end;

%% TODO no lock on main table(Bucket) is there
db_get_term_2i(Bucket, Index, Key, Option) when is_atom(Index) ->
    Fun =
        case Option of
            keys ->
                fun() ->
                        [ PKey || {_Index, _Key, PKey} <- mnesia:read(Index, Key) ]
                end;
            records ->
                fun() ->
                        [ hd(mnesia:read(Bucket, PKey)) ||
                            {_Index, _Key, PKey} <- mnesia:read(Index, Key) ]
                end
        end,
    case mnesia:transaction(Fun) of
        {atomic, Results} ->
            {ok, Results};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec db_get_term_2i(bucket(), atom() | integer(), key(), keys | records, read | write) ->
                            {ok, list(term())} | {error, timeout}.

db_get_term_2i(Bucket, Index, Key, Option, LockOption) when is_integer(Index) ->
    Fun =
        case Option of
            keys ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             element(2, Record) ||
                                Record <- mnesia:table(Bucket, [{lock, LockOption}]),
                                element(Index, Record) == Key
                            ]))
                end;
            records ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             Record ||
                                Record <- mnesia:table(Bucket, [{lock, LockOption}]),
                                element(Index, Record) == Key
                            ]))
                end
        end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end;

%% TODO no lock on main table(Bucket) is there
db_get_term_2i(Bucket, Index, Key, Option, LockOption) when is_atom(Index) ->
    Fun =
        case Option of
            keys ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             element(3, Record) ||
                                Record <- mnesia:table(Index, [{lock, LockOption}]),
                                element(2, Record) == Key
                            ]))
                end;
            records ->
                fun() ->
                        qlc:e(
                          qlc:q(
                            [
                             Record ||
                                Record <- mnesia:table(Bucket, [{lock, LockOption}]),
                                SRecord <- mnesia:table(Index, [{lock, LockOption}]),
                                element(2, SRecord) == Key,
                                element(2, Record) == element(3, SRecord)
                            ]))
                end
        end,
    case mnesia:transaction(Fun) of
        {atomic, Results} ->
            {ok, Results};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Creates or updates a term in the database.
-spec db_create_or_update_term(bucket(), key(), value(), metadata()) ->
                                      {ok, Key :: key()} | {error, term()}.
db_create_or_update_term(Bucket, Key, Value, Metadata) ->
    CacheExclusion =
        case db_functions:db_get_term(Bucket, Key) of
            {error, notfound} ->
                [];
            _ ->
                [{get_all_data, [key]}]
        end,
    case db_functions:db_store_term(Bucket, Key, Value, Metadata) of
        {ok,Key}->
            db_functions:flush_cache(Bucket, Key, CacheExclusion),
            {ok,Key};
        {error,Reason}->
            {error,Reason}
    end.

-spec db_create_term(bucket(), key(), value(), metadata(), {module(), atom()}) -> {ok, Key :: key()} | {error, term()}.
db_create_term(Bucket, Key, Value, Metadata, {Module, CheckAllowedCreationFunction}) ->
    Result = case db_functions:db_get_term(Bucket, Key) of
        {error, notfound} ->
            {ok, []};
        ExistingRecord ->
            case apply(Module, CheckAllowedCreationFunction, ExistingRecord) of
                true ->
                    {ok, [{get_all_data, [key]}]};
                false ->
                    {error, already_exists}
            end
    end,
    case Result of
        {error, _} ->
            Result;
        {ok, CacheExclusion} ->
            case db_functions:db_store_term(Bucket, Key, Value, Metadata) of
                {ok, Key}->
                    db_functions:flush_cache(Bucket, Key, CacheExclusion),
                    {ok, Key};
                {error, Reason}->
                    {error, Reason}
            end
    end.


%% @doc Updates an entry in the database. Gets updated value from the passed function.
%% The passed function should be of arity len(Args)+1, the last argument being one of
%% the current Value of the `Key', the atom `notfound', or the tuple {`Value', `Metadata'},
%% decided by whether `pass_metadata' is true or present in the `Options' list.
%% The passed function should either returns {ok, {`NewValue', `NewMetadata'}}, or `ErrorTerm'
%% In case of error, no update is done and the ErrorTerm is returned.
%% If no_create is false or not passed in the `Options' list, and the Key is not found,
%% then nothing is done and ok is returned.
%% WARNING: Only use for dict tables. For bag tables, the behaviour is undefined.
-spec db_update_term(
        bucket(),
        key(),
        {module(), atom(), list(term())},
        list({no_create, true|false} | {pass_metadata, true|false}),
        list(term())
       ) ->
                            ok | {ok, Key :: key()} | any().
db_update_term(Bucket, Key, {Module, Function, Args}, Options, _UpdateOptions) ->
    {atomic, Result} =
        mnesia:transaction(
          fun() ->
                  case mnesia:read(Bucket, Key, write) of
                      {abort, Error} ->
                          {error, Error};
                      [] ->
                          NoCreate =
                              case lists:keyfind(no_create, 1, Options) of
                                  {no_create, Val} ->
                                      Val;
                                  false ->
                                      false
                              end,
                          case NoCreate of
                              true ->
                                  ok;
                              false ->
                                  case apply(Module, Function, Args ++ [notfound]) of
                                      {ok, {NewValue, NewMetadata}} ->
                                          db_functions:db_store_term_fun(Bucket, Key, NewValue, NewMetadata);
                                      SomeError ->
                                          SomeError
                                  end
                          end;
                      ResultList ->
                          case apply(Module, Function, Args ++ [hd(ResultList)]) of
                              {ok, {NewValue, NewMetadata}} ->
                                  db_functions:db_store_term_fun(Bucket, Key, NewValue, NewMetadata);
                              SomeError ->
                                  SomeError
                          end
                  end
          end),
    Result.

%% behaviour is undefined for bag tables

-spec db_update_terms(
        bucket(),
        {atom() | integer(), key()} | list(key()),
        {module(), atom(), list(term())},
        {module(), atom(), list(term())}
       ) ->
                            {ok, list()} | {error, any()}.
db_update_terms(Bucket, InputArgs, {CheckModule, CheckFunction, CheckArgs}, {UpdateModule, UpdateFunction, UpdateArgs}) ->
    Result =
        mnesia:transaction(
            fun() ->
                CurrentRecords =
                    case InputArgs of
                        {Index, Key} ->
                            case db_functions:db_get_term_2i(Bucket, Index, Key, records, write) of
                                {ok, RecList} ->
                                    RecList;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        KeysList ->
                            lists:foldl(
                                fun(Key1, RecordsList) ->
                                    case RecordsList of
                                        {error, _SomeReason} ->
                                            RecordsList;
                                        _ListSoFar ->
                                            case mnesia:read(Bucket, Key1, write) of
                                                {abort, Reason} ->
                                                    {error, Reason};
                                                RecList ->
                                                    lists:append(RecordsList, RecList)
                                            end
                                    end
                                end,
                                [],
                                KeysList)
                    end,
                UpdateAllowed =
                    case CurrentRecords of
                        {error, _InternalReason} ->
                            CurrentRecords;
                        _ ->
                            apply(CheckModule, CheckFunction, CheckArgs ++ [CurrentRecords])
                    end,
                case UpdateAllowed of
                    ok ->
                        {ok, UpdateList} = apply(UpdateModule, UpdateFunction, UpdateArgs),
                        ResultList =
                            lists:map(
                                fun({KeyToUpdate, NewValue, NewMetadata, Operation}) ->
                                    case Operation of
                                        delete ->
                                            db_functions:db_delete_key(Bucket,KeyToUpdate);
                                        add ->
                                            {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                Bucket, KeyToUpdate, NewValue, NewMetadata);
                                        update ->
                                            {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                Bucket, KeyToUpdate, NewValue, NewMetadata)
                                    end,
                                    {Operation, KeyToUpdate, NewValue}
                                end,
                                UpdateList),
                        {ok, ResultList};
                    {error, Error} ->
                        {error, Error}
                end
            end),
    case Result of
        {aborted, ErrorTerm} ->
            {error, ErrorTerm};
        {atomic, Results} ->
            {ok,Results}

    end.

-spec db_update_create_terms(
        bucket(),
        {atom(), atom(), db_columnname(), list() | term()},
        list({bucket(), key(), value(), metadata()}),
        atom()
) -> {ok, list()} | {error, term()}.
db_update_create_terms(Bucket, UpdateArgs, OtherOperationList, {UpdateModule, UpdateFunction}) ->
    Result =
        mnesia:transaction(
            fun() ->
                CurrentRecords =
                    case UpdateArgs of
                        {index, bulk, ColumnName, ValueList} ->
                            AttrList = get_tableinfo(Bucket, attributes),
                            Index = get_index(ColumnName, AttrList) + 1,
                            lists:foldl(
                                fun(Value, RecordListAccumulator) ->
                                    case db_functions:db_get_term_2i(Bucket, Index, Value, records, write) of
                                        {ok, RecordList} ->
                                            RecordList ++ RecordListAccumulator;
                                        {error, Reason} ->
                                            mnesia:abort(Reason)
                                    end
                                end,
                                [],
                                ValueList
                            );
                        {index, unit, ColumnName, Value} ->
                            AttrList = get_tableinfo(Bucket, attributes),
                            Index = get_index(ColumnName, AttrList) + 1,
                            case db_functions:db_get_term_2i(Bucket, Index, Value, records, write) of
                                {ok, RecList} ->
                                    RecList;
                                {error, Reason} ->
                                    mnesia:abort(Reason)
                            end;
                        {sindex, bulk, Index, KeyList} ->
                            lists:foldl(
                                fun(Key, RecordListAccumulator) ->
                                    case db_functions:db_get_term_2i(Bucket, Index, Key, records, write) of
                                        {ok, RecordList} ->
                                            RecordListAccumulator ++ RecordList;
                                        {error, Reason} ->
                                            mnesia:abort(Reason)
                                    end
                                end,
                                [],
                                KeyList
                            );
                        {sindex, unit, Index, Key} ->
                            case db_functions:db_get_term_2i(Bucket, Index, Key, records, write) of
                                {ok, RecList} ->
                                    RecList;
                                {error, Reason} ->
                                    mnesia:abort(Reason)
                            end;
                        KeysList ->
                            lists:foldl(
                                fun(Key1, RecordListAccumulator) ->
                                    case mnesia:read(Bucket, Key1, write) of
                                        {abort, Reason} ->
                                            mnesia:abort(Reason);
                                        RecordList ->
                                            RecordList ++ RecordListAccumulator
                                    end
                                end,
                                [],
                                KeysList)
                    end,
                {ok, UpdateList} = apply(UpdateModule, UpdateFunction, [CurrentRecords]),
                ResultList =
                    lists:map(
                        fun(OperationDetails) ->
                            case OperationDetails of
                                {DifferentBucket, KeyToUpdate, NewValue, NewMetadata, {CheckModule, CheckFunction}, Operation} ->
                                    case Operation of
                                        add ->
                                            case apply(CheckModule, CheckFunction, [NewValue]) of
                                                true ->
                                                    {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                        Bucket, KeyToUpdate, NewValue, NewMetadata);
                                                false ->
                                                    mnesia:abort("Check for adding new data failed")
                                            end;
                                        add_different ->
                                            case apply(CheckModule, CheckFunction, [NewValue]) of
                                                true ->
                                                    {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                        DifferentBucket, KeyToUpdate, NewValue, NewMetadata);
                                                false ->
                                                    mnesia:abort("Check for adding new data failed")
                                            end;
                                        update ->
                                            case apply(CheckModule, CheckFunction, [NewValue]) of
                                                true ->
                                                    {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                        Bucket, KeyToUpdate, NewValue, NewMetadata);
                                                false ->
                                                    mnesia:abort("Check for updating data failed")
                                            end
                                    end,
                                    {Operation, KeyToUpdate, NewValue};
                                {DifferentBucket, KeyToUpdate, NewValue, NewMetadata, Operation} ->
                                    case Operation of
                                        add ->
                                            {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                Bucket, KeyToUpdate, NewValue, NewMetadata);
                                        add_different ->
                                            {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                DifferentBucket, KeyToUpdate, NewValue, NewMetadata);
                                        update ->
                                            {ok, KeyToUpdate} = db_functions:db_create_or_update_term(
                                                Bucket, KeyToUpdate, NewValue, NewMetadata)
                                    end,
                                    {Operation, KeyToUpdate, NewValue};
                                {DifferentBucket, KeyToUpdate, Operation} ->
                                    case Operation of
                                        delete ->
                                            ok = db_functions:db_delete_key(Bucket, KeyToUpdate);
                                        delete_different ->
                                            ok = db_functions:db_delete_key(DifferentBucket, KeyToUpdate);
                                        delete_sindex ->
                                            mnesia:delete(DifferentBucket, KeyToUpdate, write)
                                    end,
                                    {Operation, KeyToUpdate}
                            end
                        end,
                        UpdateList ++ OtherOperationList),
                {ok, ResultList}
            end),
    case Result of
        {aborted, ErrorTerm} ->
            {error, ErrorTerm};
        {atomic, Results} ->
            {ok,Results}

    end.

%% @doc Lists all keys in the specified bucket.
%% Uses secondary indices.
-spec db_list_keys(bucket()) ->
                          {ok, [key()]} | {error, term()}.
db_list_keys(Bucket) ->
    {atomic, Keys} =
        mnesia:transaction(
          fun() ->
                  mnesia:all_keys(Bucket)
          end),
    {ok, Keys}.

-spec delete_record_by_column(bucket(), db_columnname(), term()) -> ok | {error, term()}.
delete_record_by_column(Bucket, ColumnName, Value) ->
    AttrList = get_tableinfo(Bucket, attributes),
    Index = get_index(ColumnName, AttrList) + 1,
    Fun=
        fun() ->
            qlc:e(
                qlc:q(
                    [
                        element(2, Record) ||
                        Record <- mnesia:table(Bucket),
                        element(Index, Record) == Value
                    ]))
        end,
    case mnesia:transaction(Fun) of
        {atomic, Results} ->
            case Results of
                []->
                    {error, notfound};
                Results->
                    db_delete_keys(Bucket, Results)
            end;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Deletes a key from the database.
-spec db_delete_key(bucket(), key()) ->
                           ok | {error, term()}.
db_delete_key(Bucket, Key) ->
    mnesia:transaction(
      fun() ->
              db_functions:remove_2i_entries_fun(Bucket, Key),
              mnesia:delete(Bucket, Key, write)
      end),

    db_functions:flush_cache(Bucket,Key),
    ok.


-spec db_delete_keys(bucket(), list(key())) ->
                           ok | {error, term()}.
db_delete_keys(Bucket, KeysList) ->
    Result =
        mnesia:transaction(
            fun() ->
                lists:foreach(
                    fun(Key) ->
                        db_functions:remove_2i_entries_fun(Bucket, Key),
                        mnesia:delete(Bucket, Key, write)
                    end,
                    KeysList)
            end),
    case Result of
        {atomic, _Results} ->
          lists:foreach(
            fun(Key) ->
                        db_functions:flush_cache(Bucket,Key)
                    end,
                    KeysList
            ),

            ok;
        {aborted, Error} ->
            {error, Error}
    end.

-spec db_bulk_delete(list({bucket(), list(key())})) ->
    ok | {error, term()}.
db_bulk_delete(Items) when is_list(Items) ->
    Result =
        mnesia:transaction(
            fun() ->
                lists:foreach(
                    fun({Bucket, Keys}) ->
                        lists:foreach(
                            fun(Key) ->
                                db_functions:remove_2i_entries_fun(Bucket, Key),
                                mnesia:delete(Bucket, Key, write)
                            end,
                            Keys)
                    end,
                    Items)
            end),
    case Result of
        {atomic, _Results} ->
            lists:foreach(
                fun({Bucket, Keys}) ->
                    lists:foreach(
                        fun(Key) ->
                            db_functions:flush_cache(Bucket,Key)
                        end,
                        Keys
                    ),
                    ok
                end,
                Items
            ),
            ok;
        {aborted, Error} ->
            {error, Error}
    end.

-spec db_delete_record(bucket(), key()) ->
                           ok | {error, term()}.
db_delete_record(Bucket, Key) ->
    mnesia:transaction(
      fun() ->
              mnesia:delete_object(Bucket, Key, write)
      end),
    ok.

-spec db_exec_qlc(term()) -> {ok, list(term())} | {error, timeout}.
db_exec_qlc(Query) ->
    case mnesia:transaction(fun() -> qlc:e(Query) end) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Performs a query on postgresql database
-spec db_query_pgsql(iolist()) -> any().
db_query_pgsql(Query) ->
    case application:get_env(butler_server, pgsql_enabled, false) of
        true ->
            Pid = take_pool_member(pgsql_pool),
            Out = gen_server:call(Pid, {squery, Query}),
            return_pool_member(pgsql_pool, Pid),
            Out;
        false ->
            ok
    end.

%% @doc Performs a query on postgresql database
-spec db_query_pgsql_ext(iolist(), list()) -> any().
db_query_pgsql_ext(Query, Parameters) ->
    case application:get_env(butler_server, pgsql_enabled, false) of
        true ->
            Pid = take_pool_member(pgsql_pool),
            Out = gen_server:call(Pid, {equery, Query, Parameters}),
            return_pool_member(pgsql_pool, Pid),
            Out;
        false ->
            ok
    end.

db_delete_table(Table) ->
  AllTables = models:all(),
  TabElem = proplists:get_value(Table, AllTables),
  case TabElem of
    undefined ->
        table_not_found;
    Opts ->
        SIndexTabs = proplists:get_value(sindex, Opts),
        lists:foreach(
            fun(Elem) ->
                {SecondaryTab, _} = Elem,
                mnesia:delete_table(SecondaryTab)
            end,
            SIndexTabs),
        mnesia:delete_table(Table)
    end.

-spec db_save_riemann_event(string(), list({atom() | binary(), term()})) -> ok.
db_save_riemann_event(Series, DataList) ->
    case application:get_env(butler_server, riemann_enabled, false) of
        true ->
            {ok, HostName} = inet:gethostname(),
            InstallationId = application:get_env(butler_server, installation_id, "default"),
            DataListNew = [{"installation_id", InstallationId}] ++ DataList,
            TimeStamp = logging_utils:get_epoch_in_nanoseconds(),
            ok = katja:send_event_async([{time, TimeStamp}, {host, HostName}, {service, Series}, {tags, ["butler_server"]}, {attributes, DataListNew}]);
        false ->
            ok
    end.

-spec db_save_riemann_metric(string(), number(), list({atom() | binary(), term()})) -> ok.
db_save_riemann_metric(Series, Metric, DataList) ->
    case application:get_env(butler_server, riemann_enabled, false) of
        true ->
            {ok, HostName} = inet:gethostname(),
            InstallationId = application:get_env(butler_server, installation_id, "default"),
            DataListNew = [{"installation_id", InstallationId}] ++ DataList,
            TimeStamp = logging_utils:get_epoch_in_nanoseconds(),
            ok = katja:send_event_async([{time, TimeStamp}, {host, HostName}, {service, Series}, {tags, ["butler_server"]}, {metric, Metric}, {attributes, DataListNew}]);
        false ->
            ok
    end.

% @doc get the data by key.
-spec get_data_by_key(bucket(), key(), db_columnlist()|record) -> {ok, list()}|{error, term()}.
get_data_by_key(Bucket, Key, ColumnList) ->
    CacheName = get_cache_name(Bucket),
    CacheKey = {get_data_by_key, Key, record},
    ReturnValue = simple_cache:get(CacheName, infinity, CacheKey,
        fun() ->
            Fun =
                fun() ->
                    mnesia:read(Bucket, Key, read)
                end,
            case mnesia:transaction(Fun) of
                {atomic, Results} ->
                    {ok, Results};
                {aborted, Reason} ->
                    {error, Reason}
            end
        end),
    case ReturnValue of
        {ok, []} ->
            ReturnValue;
        {ok, Results} ->
            case ColumnList of
                record->
                    ReturnValue;
                _Other->
                    AttrList=get_tableinfo(Bucket, attributes),
                    {ok, get_columns(AttrList,ColumnList,Results)}
            end;
        _Error ->
            simple_cache:flush(CacheName, CacheKey),
            ReturnValue
    end.

-spec get_column_by_key(bucket(), key(), db_columnname()) -> {error, notfound}|{error|ok, term()}.
get_column_by_key(Bucket, Key, ColumnName) ->
    ReturnValue = db_functions:get_data_by_key(Bucket, Key, [ColumnName]),
    case ReturnValue of
        {ok, []} ->
            {error, notfound};
        {ok, [Results | _]} ->
            {ok, Results};
        _Error ->
            ReturnValue
    end.

-spec get_all_data(bucket(), db_columnlist()|record|key) -> {ok, list()}|{error, term()}.
get_all_data(Bucket, ColumnList) ->
    get_all_data_from_db(Bucket, ColumnList).

-spec get_all_data_from_db(bucket(),db_columnlist()|record|key)->{ok,list()}|{error,term()}.
get_all_data_from_db(Bucket,ColumnList) ->
    get_all_data_internal(Bucket, ColumnList).


-spec search_by(bucket(), db_whereclauselist(), db_columnlist()|record|key) -> {ok, list()}|{error, term()}.
search_by(Bucket, WhereClauseList, ColumnList) ->
    CacheName = get_cache_name(Bucket, search_by),
    CacheKey = {search_by, WhereClauseList, ColumnList},
    ReturnValue = simple_cache:get(CacheName, infinity, CacheKey,
        fun() ->
            search_by_internal(Bucket, WhereClauseList, ColumnList)
        end),
    case ReturnValue of
        {error, _Reason} ->
            simple_cache:flush(CacheName, CacheKey);
        _Success ->
            ok
    end,
    ReturnValue.

ppstaskrec_meta_search_by({SecTable, SecIndex}, key) ->
    Results = mnesia:dirty_select(SecTable, [{{'_',SecIndex,'_'}, [], ['$_']}]),
    {ok,[element(3,Rec)||Rec<-Results]}.

-spec delete_all(bucket())->ok|{error, term()}.
    delete_all(Bucket)->
    {ok,Keys}=db_list_keys(Bucket),
    lists:foreach(
          fun(Key)->
              db_delete_key(Bucket,Key)
           end,
           Keys

      ).


-spec update(bucket(),key(),{module(), atom(), list(term())},list({no_create, true|false} | {pass_metadata, true|false}),list(term()))
  ->
         ok | {ok, key()} | any().
update(Bucket, Key, {Module, Function, Args}, Options,_UpdateOptions)->
  case db_update_term(Bucket, Key, {Module, Function, Args}, Options,_UpdateOptions) of
    ok ->
        db_functions:flush_cache(Bucket,Key),
        ok;
    {ok,Key} ->
        db_functions:flush_cache(Bucket,Key),
        {ok,Key};

    {error,Reason} ->
        {error,Reason}
  end.



%%%===================================================================
%%% Internal functions
%%%===================================================================

% @doc get the data by where clause
-spec search_by_internal(bucket(),db_whereclauselist(),db_columnlist()|record|key)->{ok,list()}|{error,term()}.
search_by_internal(Bucket,WhereClauseList,ColumnList)->
  case WhereClauseList==[] of
    true->
          case get_all_data_internal(Bucket,ColumnList) of
              {error,Reason}->
                        {error,Reason};
               Data->
                      Data
           end;

    false->
        case  get_record_by_where_clause(Bucket,WhereClauseList) of
          {ok,Results} ->
              case ColumnList of
                record->
                       {ok,Results};
                key->
                        {ok,[element(2,Rec)||Rec<-Results]} ;
                _Other->
                    AttrList=get_tableinfo(Bucket,attributes),
                    {ok,get_column_list(AttrList,ColumnList,Results)}

              end;
            {error,Reason} ->
                      {error,Reason}
         end
  end.

-spec get_all_data_internal(bucket(), db_columnlist() | record | key) -> {ok, list()} | {error, term()}.
get_all_data_internal(Bucket, ColumnList) ->
    CacheName = get_cache_name(Bucket),
    CacheKey = case is_list(ColumnList) of
                   true ->
                       {get_all_data, record};
                   false ->
                       {get_all_data, ColumnList}
               end,
    ReturnValue = simple_cache:get(CacheName, infinity, CacheKey,
        fun() ->
            case
                mnesia:transaction(
                    fun() ->
                        case ColumnList of
                            key ->
                                {ok, mnesia:all_keys(Bucket)};
                            _Other ->
                                {ok, mnesia:select(Bucket, [{'_', [], ['$_']}])}
                        end
                    end
                ) of
                {atomic, Result} ->
                    Result;
                {aborted, Reason} ->
                    lager:error("Transaction aborted on Bucket ~p, ColumnList ~p due to ~p",
                        [Bucket, ColumnList, Reason]),
                    {error, Reason}
            end
        end),
    case ReturnValue of
        {ok, []} ->
            ReturnValue;
        {ok, Results} ->
            case ColumnList of
                record ->
                    ReturnValue;
                key ->
                    ReturnValue;
                _Other ->
                    AttrList = get_tableinfo(Bucket, attributes),
                    {ok, get_column_list(AttrList, ColumnList, Results)}
            end;
        _Error ->
            simple_cache:flush(CacheName, CacheKey),
            ReturnValue
    end.

get_columns(AttrList,ColumnList,Result)->
     ColPosList=
                  lists:map(
                  fun(Col)->
                      case catch get_index(Col,AttrList) of
                          N-> N+1
                       end
                  end,
                  ColumnList
              ),

      lists:map(
                fun(ColPos)->
                    element(ColPos,hd(Result))
                end,
                ColPosList
                ).

get_column_list(AttrList,ColumnList,Results)->
      ColPosList=
                      lists:map(
                      fun(Col)->
                          case catch get_index(Col,AttrList) of
                              N-> N+1
                           end
                      end,
                      ColumnList
                      ),

                     lists:map(
                      fun(Rec)->
                       lists:map(
                                  fun(ColPos)->
                                   element(ColPos,Rec)
                                   end,
                                  ColPosList
                                  )
                        end,
                        Results
                      ).


-spec get_tableinfo(bucket(),Option:: index | attributes)->list().
get_tableinfo(Bucket,Option)->
    mnesia:table_info(Bucket, Option).

-spec get_index(Item::term(),List::list())->any().
get_index(Item,List) ->
    get_index(Item,List,1).
get_index(Item,[Item|_],N) ->
    N;
get_index(Item,[_|T],N) ->
    get_index(Item,T,N+1).

search_by_all(Bucket, AttrList, WhereClauseList) ->
      fun() ->
        qlc:e(
          lists:foldl(
            fun({Col,Operator,Val}, Acc) ->
                    case lists:member(Col,AttrList) of
                        true ->
                            qlc:q(
                              [
                               Record ||
                                  Record <- Acc,
                                  case Operator of
                                  equal ->
                                      element(get_index(Col,AttrList)+1, Record) == Val;
                                  lessthanorequalto ->
                                       element(get_index(Col,AttrList)+1, Record) =< Val;
                                   grtthanorequalto ->
                                       element(get_index(Col,AttrList)+1, Record) >= Val;
                                  grtthan->
                                        element(get_index(Col,AttrList)+1, Record) > Val;
                                  lessthan->
                                      element(get_index(Col,AttrList)+1, Record) < Val;
                                  notequal->
                                      element(get_index(Col,AttrList)+1, Record) /= Val;
                                  in ->
                                      lists:member(element(get_index(Col,AttrList)+1, Record), Val);
                                  notin ->
                                      not lists:member(element(get_index(Col,AttrList)+1, Record), Val)
                                  end
                              ]);

                        false ->
                            qlc:q(
                            [
                             Record ||
                                Record <- Acc,
                                SRecord <- mnesia:table(Col),
                                case Operator of
                                  equal ->
                                       element(2, SRecord) == Val;
                                  lessthanorequalto ->
                                       element(2, SRecord) =< Val;
                                  grtthanorequalto ->
                                       element(2, SRecord) >= Val;
                                  grtthan->
                                       element(2, SRecord) > Val;
                                  lessthan->
                                       element(2, SRecord) > Val;
                                  notequal->
                                        element(2,SRecord)/=Val;
                                  in ->
                                        lists:member(element(2,SRecord), Val);
                                  notin ->
                                        not lists:member(element(2,SRecord), Val)
                                  end,
                                element(2, Record) == element(3, SRecord)
                            ])
                    end
            end,
            mnesia:table(Bucket),
            WhereClauseList
           ),
          cache_all
         )
      end.

%% @doc Creates a MatchPattern which can be passed to mnesia:select.
%% e.g. call: mnesia:select(person, {person, '_', 36, '_', '_'})
make_match_pattern(RecordType, AttrList, WhereClauseList) ->
    FieldValueDict = [{FieldName, FieldValue} || {FieldName, equal, FieldValue} <- WhereClauseList],

    % Look up the record name and fields in record list
    Body =
      lists:map(
        fun(Field) ->
            proplists:get_value(Field, FieldValueDict, '_')
        end,
        AttrList
        ),

    MatchHead = list_to_tuple([RecordType | Body]),
    MatchPattern = [{MatchHead, [], ['$_']}],
    MatchPattern.

%% @doc return records after matching values on secondary columns
%% A much lighter weight version of search_by which uses built-in mnesia functions
%% and hopefully uses the best indexes(internally handled by mnesia, so can't be guaranteed)
search_by_equality(RecordType, AttrList, WhereClauseList) ->
    MatchPattern = make_match_pattern(RecordType, AttrList, WhereClauseList),
    fun() ->
        mnesia:select(RecordType, MatchPattern)
    end.

-spec get_record_by_where_clause(bucket(), db_whereclauselist())->any().
get_record_by_where_clause(RecordType, WhereClauseList)->
    AttrList = get_tableinfo(RecordType, attributes),

    SimpleSearchCheck =
      lists:all(
        fun({Field, Operation, _Value}) ->
            Operation =:= equal andalso lists:member(Field, AttrList)
        end,
        WhereClauseList
        ),

    Fun =
      case SimpleSearchCheck of
          true ->
              search_by_equality(RecordType, AttrList, WhereClauseList);
          false ->
              case WhereClauseList of
                  [{Field, equal, Value}] ->
                      %% This means it's a 2i field with single where clause
                      %% with `equal' operation.
                      fun() ->
                              [ hd(mnesia:read(RecordType, PKey)) ||
                                  {_Index, _Key, PKey} <- mnesia:read(Field, Value) ]
                      end;
                  _ ->
                      search_by_all(RecordType, AttrList, WhereClauseList)
              end
      end,

    case mnesia:transaction(Fun) of
      {atomic, Results} ->
          {ok, Results};
      {aborted, Reason} ->
          {error, Reason}
    end.


-spec get_cache_name(bucket()) -> atom().
get_cache_name(Bucket) ->
    get_cache_name(Bucket, default).

-spec get_cache_name(bucket(), atom()) -> atom().
get_cache_name(Bucket, default) ->
    list_to_atom(atom_to_list(Bucket) ++ "_cache");

get_cache_name(Bucket, Arg) ->
    list_to_atom(atom_to_list(Bucket) ++ "_" ++ atom_to_list(Arg) ++ "_cache").

-spec clear_cache(bucket(), key()) -> ok.
clear_cache(Bucket, Key)->
    clear_cache(Bucket, Key, []).

-spec clear_cache(
        bucket(), key(),
        list({CacheType :: atom(), Exclusions :: list()}))
                 -> ok.
clear_cache(Bucket, Key, ExclusionList)->
    CacheName = get_cache_name(Bucket),
    FlushRules =
        [
         {get_data_by_key, []},
         {get_column_by_key, []},
         {get_all_data, [{'_', '=>', [record, key]}]},
         {search_by, []}
        ],
    CachesToFlush =
        [
         {get_data_by_key, ['_']},
         {get_column_by_key, ['_']},
         {get_all_data, ['_']},
         {search_by, ['_']}
        ],
    SimpleCacheArg =
        fun(CacheType, Arg) ->
                case CacheType of
                    get_data_by_key ->
                        {{get_data_by_key, Key, Arg}, '_', '_', '_'};
                    get_column_by_key ->
                        {{get_column_by_key, Key, Arg}, '_', '_', '_'};
                    get_all_data ->
                        {{get_all_data, Arg}, '_', '_', '_'};
                    search_by ->
                        {{search_by, Arg, '_'}, '_', '_', '_'}
                end
        end,
    [] =
        lists:foldl(
          fun({CacheType, FlushList}, ExclusionAcc) ->
                  {FinalFlushList, ExclusionAcc1} =
                      case lists:keytake(CacheType, 1, ExclusionAcc) of
                          false ->
                              {FlushList, ExclusionAcc};
                          {value, {CacheType, Exclusions}, ExclusionAcc2} ->
                              {CacheType, RuleList} =
                                  lists:keyfind(CacheType, 1, FlushRules),
                              FinalFlushList1 =
                                  expand_rule_list(FlushList, RuleList) --
                                  expand_rule_list(Exclusions, RuleList),
                              {FinalFlushList1, ExclusionAcc2}
                      end,
                  lists:foreach(
                    fun(Arg) ->
                            simple_cache:clear(CacheName,
                                               SimpleCacheArg(CacheType, Arg))
                    end,
                    FinalFlushList
                   ),
                  ExclusionAcc1
          end,
          ExclusionList,
          CachesToFlush
         ),
    ok.

-spec flush_cache(bucket(), key()) -> ok.
flush_cache(Bucket, Key) ->
    flush_cache(Bucket, Key, []).

%% @doc Flushes entries from simple cache w.r.t. a record.
%% The main cache table created for a record is divided into logical buckets.
%% Records of these logical buckets are removed by generating the keys for the same.
%% For special cache buckets(like search_by) which have a dedicated ets table are completely,
%% cleared(justifying the purpose for which a separate table was dedicated to it).
%% The difference between this function and db_functions:clear_cache() is that the latter uses pattern matching to
%% remove the rows, while this function removes entries by keys. Keys are generated using the parser SimpleCacheKey().
%% ExclusionList consists of the list buckets which would not be removed from the cache.
-spec flush_cache(
        bucket(), key(),
        list({CacheType :: atom(), Exclusions :: list()}))
            -> ok.
flush_cache(Bucket, Key, ExclusionList) ->
    CachesToFlushPartially =
        [
            {get_data_by_key, default, [record]},
            {get_all_data, default, [record, key]}
        ],
    CachesToFlushCompletely = [
        search_by
    ],
    SimpleCacheKey =
        fun(CacheType, Arg) ->
            case CacheType of
                get_data_by_key ->
                    {get_data_by_key, Key, Arg};
                get_all_data ->
                    {get_all_data, Arg}
            end
        end,
    lists:foreach(
        fun({CacheType, CacheNameArg, FlushList}) ->
            UpdatedFlushList = case lists:keyfind(CacheType, 1, ExclusionList) of
                false ->
                    FlushList;
                {CacheType, Exclusions} ->
                    FlushList -- Exclusions
            end,
            CacheName = get_cache_name(Bucket, CacheNameArg),
            lists:foreach(
                fun(Arg) ->
                    CacheKey = SimpleCacheKey(CacheType, Arg),
                    simple_cache:flush(CacheName, CacheKey)
                end,
                UpdatedFlushList
            )
        end,
        CachesToFlushPartially
    ),
    lists:foreach(
        fun(CacheType) ->
            CacheName = get_cache_name(Bucket, CacheType),
            simple_cache:flush(CacheName)
        end,
        CachesToFlushCompletely
    ),
    ok.

-spec expand_rule_list(list(atom()), list(atom())) -> list().
expand_rule_list(Rules, RuleList) ->
    ExpandRule =
        fun(Rule) ->
                case lists:keyfind(Rule, 1, RuleList) of
                    false ->
                        notfound;
                    {Rule, '=>', Expansion} ->
                        Expansion
                end
        end,
    lists:flatten(
      lists:map(
        fun(Rule) ->
                case ExpandRule(Rule) of
                    notfound ->
                        [Rule];
                    Rules1 ->
                        expand_rule_list(Rules1, RuleList)
                end
        end,
        Rules
       )
     ).
