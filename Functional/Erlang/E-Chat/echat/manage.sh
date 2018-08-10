#!/bin/bash

erl -pa ebin -sname chatsv -config "./rel/sys.config" -run echat
