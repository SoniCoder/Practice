src/client_api.erl:: src/headers.hrl; @touch $@
src/echat.erl:: src/headers.hrl; @touch $@
src/models/clientinfo.erl:: src/models/baseinfo.erl; @touch $@
src/models/message.erl:: src/models/baseinfo.erl; @touch $@
src/models/scriber.erl:: src/models/baseinfo.erl; @touch $@
src/tabmgr.erl:: src/headers.hrl; @touch $@

COMPILE_FIRST += models/baseinfo
