-module(pecypc_log).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([info/1, error/1]).

%% -----------------------------------------------------------------------------
%% Poorman logger
%% @todo replace someday with lager
%% -----------------------------------------------------------------------------

info(X) ->
  error_logger:info_report(X).

error(X) ->
  error_logger:error_report(X).
