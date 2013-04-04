@echo off
start werl -env ERL_LIBS ".;deps" -name pecypc@127.0.0.1 -boot start_sasl -sasl errlog_type error -s pecypc_app -config priv/app
