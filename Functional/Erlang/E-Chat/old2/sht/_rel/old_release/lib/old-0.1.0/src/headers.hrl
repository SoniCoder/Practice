
-record(message, {timestamp, name, text, private=false}).

-type value()                    :: on | off | term().
-type bucket()                   :: atom().
-type metadata()                 :: list({bucket(), key()}).
-type output()                   :: term().
-type key()                      :: term().

-type db_operator()              :: atom().
-type db_whereclauselist()       :: list({atom(),db_operator(),term()}).
-type db_columnlist()            :: list(atom()).
-type db_columnname()            :: atom().