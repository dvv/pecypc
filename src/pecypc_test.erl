-module(pecypc_test).

-behaviour(pecypc_api_handler).
-export([
    delete/2,
    get/2,
    put/3
  ]).

% backend
-behaviour(saddle_backend).
-export([
    authorize_client_credentials/3,
    authorize_username_password/3,
    verify_redirection_uri/2,
    register_client/4,
    validate_client/4,
    register_token/2,
    validate_token/2
  ]).

-export([handle/3]).

-include("pecypc_api.hrl").

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  {ok, <<"GOT">>}.

put(B, Q, O) ->
pecypc_log:info({put, B, Q, O}),
  % ok.
  % {ok, <<"PUT">>}.
  % {new, <<"foo">>}.
  % {error, <<"PUT">>}.
  {_, {Secret, Ttl}} = lists:keyfind(security, 1, O),
  {ok, [{bearer, termit:encode_base64({{user, <<"dvv">>}, [
        <<"admin.add">>, <<"admin.PUT">>, <<"admin.GET">>, <<"user.*">>
      ]}, Secret, Ttl)}]}.

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
  {error, badarg};
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
  {error, badarg};
authorize_client_credentials(ClientId, implicit, Scope) ->
  {ok, {client, ClientId}, Scope};
authorize_client_credentials(ClientId, _ClientSecret, Scope) ->
  {ok, {client, ClientId}, Scope}.

verify_redirection_uri(ClientId, RedirectUri) ->
  % {error, mismatch}.
  % {error, badarg}.
  ok.

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
  termit:encode_base64({client, Identity, RedirectUri, Scope}, Secret, Ttl);
register_client(Identity, RedirectUri, Scope, Secret) ->
  termit:encode_base64({client, Identity, RedirectUri, Scope}, Secret).

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
  case termit:decode_base64(ClientId, Secret) of
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
  termit:encode_base64(Data, Secret, Ttl);
register_token(Data, Secret) ->
  termit:encode_base64(Data, Secret).

%%
%% Validate token and retrieve info.
%%
validate_token(Token, Secret) ->
  termit:decode_base64(Token, Secret).
