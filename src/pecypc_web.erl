-module(pecypc_web).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-define(SERVER, ?MODULE).

-export([start_link/0, start/0, stop/0, reload/0]).

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
%% -----------------------------------------------------------------------------

reload() ->
  {ok, _, ProtoOpts} = config(),
  ranch:set_protocol_options(?SERVER, ProtoOpts).

%% -----------------------------------------------------------------------------
%% Get server configuration
%% -----------------------------------------------------------------------------

config() ->
  % transport options
  TransOpts = transport(),
  % NB: honor heroku environment
  TransOpts2 = case os:getenv("PORT") of
    false ->
      TransOpts;
    Port ->
      [{port, list_to_integer(Port)} | lists:keydelete(port, 1, TransOpts)]
  end,
  TransOpts3 = case os:getenv("HOST") of
    false ->
      TransOpts2;
    Host ->
      [{host, Host} | lists:keydelete(host, 1, TransOpts2)]
  end,
  % protocol options
  ProtoOpts = protocol(),
% pecypc_log:info({proto, ProtoOpts}),
  {ok, TransOpts3, ProtoOpts}.

transport() -> [
  % Port number to listen to
  {port, 3000},
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
    cowboy_session,               % requires session_opts in environment
    cowboy_handler                % process request
  ]},

  % Request environment
  {env, [
    {session_opts, pecypc_app:key(session_opts)},
    % dispatch rules
    {dispatch, dispatch()}
  ]}
].

dispatch() ->
  cowboy_router:compile([{'_', lists:flatten(routes())}]).

routes() -> [
  {"/api/user[/:id]", pecypc_api, [
    {handler, pecypc_user},
    {security, {<<"!cowboyftw!">>, 86400}},
    % {allow, [<<"GET">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"HEAD">>]},
    {scope, <<"user">>}
  ]},

  {"/api/:bucket[/:id]", pecypc_api, [
    {handler, pecypc_test},
    {security, {<<"!cowboyftw!">>, 86400}},
    % {allow, [<<"GET">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"HEAD">>]},
    {scope, <<"admin">>}
  ]},

  % oauth2 server
  {"/oauth2", oauth2_provider, [
    {backend, pecypc_test},
    {code_secret, <<"?cowboyftw?">>},
    {code_ttl, 60},
    {token_secret, <<"!cowboyftw!">>},
    {token_ttl, 3600},
    {refresh_secret, <<"@cowboyftw@">>},
    {refresh_ttl, 86400}
  ]},

  % oauth2 client helper
  [{"/auth/" ++ atom_to_list(P) ++ "/:action", cowboy_social, O}
        || {P, O} <- pecypc_app:key(social_providers)],
  % oauth2 profile helper
  [{"/api/" ++ atom_to_list(P) ++ "/:action", cowboy_social_profile, [{provider, P} | O]}
        || {P, O} <- pecypc_app:key(social_providers)],

  % static content: /* -> /priv/html/*
  {"/[...]", cowboy_static, [
    %{directory, "priv/www"},
    {directory, {priv_dir, pecypc, [<<"www">>]}},
    % @todo get rid of gen_server-ish mimetypes
    {mimetypes, { {mimetypes, path_to_mimes}, default} }
  ]}
].
