-module(pecypc_app).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).

-export([key/1]).

%% -----------------------------------------------------------------------------
%% Application starter, ensures dependencies are started.
%% Allows for `erl -s pecypc_app` to start the world.
%% -----------------------------------------------------------------------------

start() ->
  start(pecypc).

start(App) ->
  % pecypc_log:info({starting, App}),
  case application:start(App) of
      {error, {not_started, Dep}} ->
        ok = start(Dep),
        ok = start(App);
      {error,{already_started, App}} ->
        ok;
      ok ->
        ok;
      Other ->
        pecypc_log:error({error_during_start, Other}),
        Other
    end.

%% -----------------------------------------------------------------------------
%% Application is starting
%% -----------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  % start supervisor
  supervisor:start_link({local, pecypc_sup}, pecypc_sup, []).

%% -----------------------------------------------------------------------------
%% Application is stopped
%% -----------------------------------------------------------------------------

stop(_State) ->
  ok.

%% -----------------------------------------------------------------------------
%% Application configuration
%% -----------------------------------------------------------------------------

config() ->
  {ok, Cwd} = file:get_cwd(),
  {ok, Config} = file:consult(Cwd ++ "/app.config"),
  Config.

key(Key) ->
  case application:get_env(pecypc, Key) of
    {ok, Value} -> Value;
    undefined ->
      {_, Value} = lists:keyfind(Key, 1, config()),
      Value
  end.

% key(Key, Default) ->
%   case lists:keyfind(Key, 1, config()) of
%     {_, Value} -> Value;
%     false -> Default
%   end.
