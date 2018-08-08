-module(message).
-behaviour(baseinfo).

-include("../headers.hrl").

-export ([create_or_update/3,
          get_all/0,
          get_all/1,
          get_by_id/1,
          get_by_id/2,
          delete_all/0,
          delete/1,
          update/4,
          search_by/2,
          get_column_by_id/3,
          get_column_by_id/2,
          update_columns_by_id/2
]).

%-spec create_or_update(seat_name(), pps_seat()|undefined, metadata()) ->
%                               {ok, Key :: key()} | {error, term()}.
create_or_update(_TimeStamp, Message, _Metadata)->
    F = fun() ->
        mnesia:write(Message)
    end,
    mnesia:transaction(F).
    %Ret = baseinfo:create(?MODULE, SeatName, SeatRecord, Metadata),
    % case Ret of
    %     {ok, _} ->
    %         ok;%send_pps_seat_data_to_interface(SeatRecord, post);
    %     {error, _Reason} -> ok
    % end,
    %Ret.

-spec delete_all()->ok|{error,term()}.
delete_all()->
    baseinfo:delete_all(?MODULE).

%-spec delete(seat_name())->ok|{error,term()}.
delete(SeatName)->
    baseinfo:delete(?MODULE,SeatName).

-spec update(key(),{module(), atom(), list(term())},
             list({no_create, true|false} | {pass_metadata, true|false}),
             list(term()))->
                    ok | {ok, key()} | any().
update(Key, {Module, Function, Args}, Options, _UpdateOptions)->
    baseinfo:update(?MODULE, Key, {Module, Function, Args},
                    Options, _UpdateOptions).

%-spec get_all()->{ok, list(pps_seat())} | {error, term()}.
get_all()->
    baseinfo:get_all(?MODULE).

-spec get_all(db_columnlist()|key)->{ok,list()}|{error,term()}.
get_all(ColumList)->
    baseinfo:get_all(?MODULE,ColumList).


%-spec get_by_id(seat_name())->{ok,pps_seat()}|{error,term()}.
get_by_id(SeatName)->
    baseinfo:get_by_id(?MODULE,SeatName).

%-spec get_by_id(seat_name(),db_columnlist())->{ok,list()}|{error,term()}.
get_by_id(SeatName,ColumnList)->
    baseinfo:get_by_id(?MODULE,SeatName,ColumnList).


%-spec get_column_by_id(seat_name(),db_columnname())->
%                              {ok,term()}|{error,term()}.
get_column_by_id(SeatName,ColumnName)->
    baseinfo:get_column_by_id(?MODULE,SeatName,ColumnName).


%-spec get_column_by_id(seat_name(),db_columnname(),term())->
%                              term()|{error|term()}.
get_column_by_id(SeatName,ColumnName,DefaultValue)->
    baseinfo:get_column_by_id(?MODULE, SeatName, ColumnName, DefaultValue).


-spec search_by(db_whereclauselist(),db_columnlist()|record|key)->
                       {ok,list()}|{error,term()}.
search_by(WhereClauseList,ColumnList)->
    baseinfo:search_by(?MODULE,WhereClauseList,ColumnList).

-spec update_columns_by_id(key(),list({atom(),term()}))->
                                  {ok,key()}|{error,term()}.
update_columns_by_id(Key,List)->
    List = xD,
    Result =
        mnesia:transaction(
          fun() ->
                %   {ok, PpsSeat} = db_functions:db_get_term(?MODULE, Key),
                %   AttrList = mnesia:table_info(?MODULE,attributes),
                % %   PpsSeatNew =
                %       lists:foldl(
                %         fun({Col,Value},Acc)->
                %                 setelement(db_functions:get_index(
                %                              Col, AttrList)+1, Acc,Value)
                %         end,
                %         PpsSeat,
                %         List),
                  %Metadata = get_metadata(PpsSeatNew),
                  %{ok, Key} = db_functions:db_create_or_update_term(?MODULE,Key,PpsSeatNew,Metadata),
                  %{ok, Key}
                  ok
          end
         ),
    case  Result of
        {atomic,{ok,Key}} ->
            {ok,Key};
        {aborted,Reason}->
            {error,Reason}
    end.
