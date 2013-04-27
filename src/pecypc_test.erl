-module(pecypc_test).

-behaviour(pecypc_api_handler).
-export([
    % allowed/2,
    authorize/3,
    delete/2,
    get/2,
    put/3
  ]).

% backend
-behaviour(saddle_backend).
-export([
    authorize_client_credentials/3,
    authorize_username_password/3,
    get_redirection_uri/1,
    register_client/4,
    validate_client/4,
    register_token/2,
    validate_token/2
  ]).

-export([handle/3]).

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

authorize(Type, Credentials, _Options) ->
pecypc_log:info({authorize, Type, Credentials, _Options}),
  % termit:verify_token()
  {ok, {<<"foo">>, <<"you-are-admin.*">>}}.

%%
%% Return true if Method of BaseScope is in AllowedScope for Identity,
%% according to https://github.com/kivra/oauth2#scope.
%%
allowed(_, none) ->
  false;
allowed(Method, {_Identity, Scope}) ->
pecypc_log:info({allowed, Method, Scope}),
  BaseScope = <<"you-are-admin">>,
  oauth2_priv_set:is_member([BaseScope, Method], oauth2_priv_set:new(Scope)).

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  {ok, <<"GOT">>}.

put(B, Q, O) ->
pecypc_log:info({put, B, Q, O}),
  % ok.
  {ok, <<"PUT">>}.
  % {new, <<"foo">>}.
  % {error, <<"PUT">>}.
  % {_, {Secret, Ttl}} = lists:keyfind(security, 1, O),
  % {ok, [{bearer, termit:issue_token({{user, <<"dvv">>}, [
  %       <<"admin.add">>, <<"admin.PUT">>, <<"admin.GET">>, <<"user.*">>
  %     ]}, Secret, Ttl)}]}.

delete(Q, O) ->
pecypc_log:info({delete, Q, O}),
  {error, [{foo, <<"bar">>}]}.

handle(<<"GET">>, [Q], O) ->
  get(Q, O);

handle(<<"PUT">>, [B, Q], O) ->
  put(B, Q, O);

handle(<<"PATCH">>, [B, Q], O) ->
  put(B, Q, O);

handle(<<"DELETE">>, [Q], O) ->
  delete(Q, O);

handle(<<"add">>, [X, Y], _O) ->
pecypc_log:info({add, X, Y}),
  {ok, X + Y}.

%%
%%------------------------------------------------------------------------------
%% OAuth2 backend functions
%%------------------------------------------------------------------------------
%%

%%
%% Validate username and password of resource owner for given scope.
%%
authorize_username_password(_, _, undefined) ->
  {error, scope};
authorize_username_password(Username, Password, Scope)
  when   Username =:= undefined
  orelse Password =:= undefined ->
  {error, notfound};
authorize_username_password(Username, Password, Scope) ->
  % {error, badarg}.
  % {error, mismatch}.
  % {error, scope}.
  {ok, {user, Username}, Scope}.

%%
%% Validate client credentials for given scope.
%%
authorize_client_credentials(_, _, undefined) ->
  {error, scope};
authorize_client_credentials(ClientId, ClientSecret, Scope)
  when   ClientId =:= undefined
  orelse ClientSecret =:= undefined ->
  {error, notfound};
authorize_client_credentials(ClientId, implicit, Scope) ->
  {ok, {client, ClientId}, Scope};
authorize_client_credentials(ClientId, _ClientSecret, Scope) ->
  {ok, {client, ClientId}, Scope}.

get_redirection_uri(ClientId) ->
  % {error, notfound}.
  {ok, <<"https://a6cypg.hopto.org/auth/native/callback">>}.

%%
%% Register a new client.
%%
register_client(Identity, RedirectUri, Scope, Options)
    when Identity =:= undefined
    orelse RedirectUri =:= undefined
    orelse Scope =:= undefined
    orelse Options =:= undefined ->
  {error, badarg};
register_client(Identity, RedirectUri, Scope, {Secret, Ttl}) ->
  termit:issue_token({client, Identity, RedirectUri, Scope}, Secret, Ttl).

%%
%% Get info on given client.
%%
validate_client(ClientId, RedirectUri, Scope, Secret)
    when ClientId =:= undefined
    orelse RedirectUri =:= undefined
    orelse Scope =:= undefined
    orelse Secret =:= undefined ->
  {error, badarg};
validate_client(ClientId, RedirectUri, Scope, Secret) ->
  case termit:verify_token(ClientId, Secret) of
    {ok, {client, Name, RedirectUri, Scope}} ->
      {ok, Name};
    {ok, {client, _, _, _}} ->
      {error, badarg};
    {ok, _} ->
      {error, badarg};
    {error, _} ->
      {error, badarg}
  end.

%%
%% Generate token.
%%
register_token(Data, {Secret, Ttl}) ->
  termit:issue_token(Data, Secret, Ttl).

%%
%% Validate token and retrieve info.
%%
validate_token(Token, Secret) ->
  termit:verify_token(Token, Secret).
