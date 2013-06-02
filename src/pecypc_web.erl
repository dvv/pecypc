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
  TransOpts = transport(),
  % NB: honor foreign environment
  TransOpts2 = case os:getenv("PORT") of
    false -> TransOpts;
    Port -> [{port, list_to_integer(Port)} |
                lists:keydelete(port, 1, TransOpts)]
  end,
  TransOpts3 = case os:getenv("HOST") of
    false -> TransOpts2;
    Host -> [{host, Host} | lists:keydelete(host, 1, TransOpts2)]
  end,
  % protocol options
  ProtoOpts = protocol(),
% pecypc_log:info({proto, ProtoOpts}),
  {ok, TransOpts3, ProtoOpts}.

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
    cowboy_session,               % requires session_opts in environment
    % cowboy_bearer,                % requires bearer_opts in environment
    cowboy_handler                % process request
  ]},

  % Request environment
  {env, [
    {session_opts, pecypc_app:key(session)},
    {bearer_opts, pecypc_app:key(bearer)},
    % dispatch rules
    {dispatch, dispatch()}
  ]}
].

dispatch() ->
  cowboy_router:compile([{'_', lists:flatten(routes())}]).

routes() -> [
  {"/api/:bucket[/:id]", pecypc_test, [{token_secret, <<"!cowboyftw!">>}]},

  {"/auth", cowboy_social_loginza, [
    {id, <<"52001">>},
    {secret, <<"4e78bf1e3cce0d799c32d6bb93e79465">>}
  ]},

  % oauth2 client helper
  [{"/auth/" ++ atom_to_list(P) ++ "/:action", cowboy_social, O}
        || {P, O} <- pecypc_app:key(social_providers)],
  % oauth2 profile helper
  [{"/api/" ++ atom_to_list(P) ++ "/:action", cowboy_social_profile, [{provider, P} | O]}
        || {P, O} <- pecypc_app:key(social_providers)],

  % static content: /* -> /priv/www/*
  {"/", cowboy_static, [
    {directory, {priv_dir, pecypc, [<<"www">>]}},
    {file, <<"index.html">>},
    {mimetypes, { {mimetypes, path_to_mimes}, default} }
  ]},
  {"/[...]", cowboy_static, [
    {directory, {priv_dir, pecypc, [<<"www">>]}},
    {mimetypes, { {mimetypes, path_to_mimes}, default} }
  ]}
].
