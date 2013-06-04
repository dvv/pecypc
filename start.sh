#!/bin/sh
# windows
if test "x$USERPROFILE" != x; then
  DIRSEP=';'
  ERL='start werl'
# *nix
else
  DIRSEP=':'
  ERL=erl
fi
APP=${PWD##*/}
# -eval "${APP}_app:start()."
${ERL} -env ERL_LIBS ".${DIRSEP}deps" +K true +P 4000000 -smp auto \
  -name ${APP}@127.0.0.1 \
  -boot start_sasl -sasl errlog_type error \
  -s "${APP}_app" \
  -config priv/app
