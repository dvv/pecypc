-module(pecypc_web).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-define(SERVER, ?MODULE).

-export([
    reload/0,
    start/0,
    start_link/0,
    stop/0
  ]).

%% -----------------------------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------------------------

start_link() ->
  start().

start() ->
  {ok, TransOpts, ProtoOpts} = config(),
  cowboy:start_http(?SERVER, 128, TransOpts, ProtoOpts).

stop() ->
  cowboy:stop_listener(?SERVER).

%% -----------------------------------------------------------------------------
%% Hot-reload protocol options
%%
%% NB: use this exported function to update running configuration
%% -----------------------------------------------------------------------------

reload() ->
  {ok, _, ProtoOpts} = config(),
  ranch:set_protocol_options(?SERVER, ProtoOpts).

%% -----------------------------------------------------------------------------
%% Get server configuration
%% -----------------------------------------------------------------------------

config() ->
  % transport options
  % NB: honor foreign environment
  TransOpts =
      env(host, "HOST",
      env(port, "PORT", transport(), fun list_to_integer/1)),
  % protocol options
  ProtoOpts = protocol(),
  {ok, TransOpts, ProtoOpts}.

env(OptKey, EnvKey, Opts, ConvertFun) ->
  case os:getenv(EnvKey) of
    false -> Opts;
    Value -> [{OptKey, ConvertFun(Value)} | lists:keydelete(OptKey, 1, Opts)]
  end.
env(OptKey, EnvKey, Opts) ->
  env(OptKey, EnvKey, Opts, fun (X) -> X end).

%% -----------------------------------------------------------------------------
%% Configuration
%% -----------------------------------------------------------------------------

transport() -> [
  % Port number to listen to
  {port, 3000},
  % {port, 443},
  % % {cacertfile, "priv/ssl/cowboy-ca.crt"},
  % {certfile, "priv/ssl/server.crt"},
  % {keyfile, "priv/ssl/server.key"},
  % Interface to listen on
  %{ip, <<"192.168.0.1">>},
  % Maximum number of simultaneous connections
  {max_connections, infinity},
  % Maximum length of the pending connections queue
  {backlog, 8192}
].

protocol() -> [
  % Maximum number of requests allowed in a single keep-alive session
  {max_keepalive, infinity},

  % Request preprocessors
  {middlewares, [
    cowboy_router,                % determine handler and its options
    cowboy_handler                % process request
  ]},

  % Request environment
  {env, [
    {dispatch, router()}
  ]}
].

% root() ->
%   pecypc_log:info({roor}),
%   <<"c:/dev/pecypc/priv/www">>.

router() ->
  cowboy_router:compile([{'_', [
    {"/api/:bucket/[:id]", pecypc_test, []},
    % {"/api/dav/[...]", pecypc_dav, [{root, <<"/dav">>}, {auth, {<<"dvv">>, <<"dvv">>}}]},
    % static content: /* -> /priv/www/*
    {"/", cowboy_static, [
      {directory, {priv_dir, pecypc, [<<"www">>]}},
      {file, <<"index.html">>},
      {mimetypes, [{<<".html">>, [{<<"text">>, <<"html">>, []}]}]}
    ]},
    {"/[...]", cowboy_static, [
      {directory, {priv_dir, pecypc, [<<"www">>]}},
      {mimetypes, {{mimetypes, path_to_mimes}, default}}
    ]}
  ]}]).
