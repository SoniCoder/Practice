
-record(message, {
    time                    :: time(),
    timestamp               :: string(), 
    name                    :: string(),
    text                    :: string(),
    private=false           :: 'true' | 'false',
    receiver=broadcast      :: string()}).


-record(scriber, {
    pid                     :: pid(), 
    ref                     :: reference()}).
-record(clientinfo, {
    name                    :: string(),
    id                      :: clientid()}).

-type value()                    :: on | off | term().
-type bucket()                   :: atom().
-type metadata()                 :: list({bucket(), key()}).
-type output()                   :: term().
-type key()                      :: term().

-type db_operator()              :: atom().
-type db_whereclauselist()       :: list({atom(),db_operator(),term()}).
-type db_columnlist()            :: list(atom()).
-type db_columnname()            :: atom().

-type time()                     :: calendar:time().

-type clientid()                 :: #scriber{}.
-type message()                  :: #message{}.
-type clientinfo()               :: #clientinfo{}.