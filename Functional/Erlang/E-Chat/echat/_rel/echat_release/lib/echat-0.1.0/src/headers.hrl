
-record(message, {time, timestamp, name, text, private=false}).
-record(scriber, {pid, ref}).
-record(clientinfo, {name, id}).

-type value()                    :: on | off | term().
-type bucket()                   :: atom().
-type metadata()                 :: list({bucket(), key()}).
-type output()                   :: term().
-type key()                      :: term().

-type db_operator()              :: atom().
-type db_whereclauselist()       :: list({atom(),db_operator(),term()}).
-type db_columnlist()            :: list(atom()).
-type db_columnname()            :: atom().