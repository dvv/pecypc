-module(pecypc_log).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-compile([{parse_transform, lager_transform}]).

-export([info/1, warn/1, error/1]).

%% -----------------------------------------------------------------------------
%% Poorman logger
%% @todo replace someday with lager
%% -----------------------------------------------------------------------------

info(X) ->
  lager:info("~p", [X]).
  % error_logger:info_report(X).

warn(X) ->
  lager:warning("~p", [X]).
  % error_logger:info_report(X).

error(X) ->
  lager:error("~p", [X]).
  % error_logger:error_report(X).
