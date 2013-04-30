-module(pecypc_log).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-compile([{parse_transform, lager_transform}]).

-export([info/1, warn/1, error/1]).

info(X) ->
  lager:info("~p", [X]).

warn(X) ->
  lager:warning("~p", [X]).

error(X) ->
  lager:error("~p", [X]).
