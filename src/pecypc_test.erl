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
    authorize_client_credentials/4,
    authorize_username_password/3,
    register_client/4,
    validate_client/2,
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
authorize_username_password(Username, Password, _) when
    Username =:= undefined; Password =:= undefined ->
  {error, mismatch};
authorize_username_password(Username, Password, Scope) ->
  {ok, {user, Username}, Scope}.

%%
%% Validate client credentials for given scope.
%%
authorize_client_credentials(_, undefined, _, _) ->
  {error, redirect_uri};
authorize_client_credentials(_, _, _, undefined) ->
  {error, scope};
authorize_client_credentials(ClientId, _, ClientSecret, _) when
    ClientId =:= undefined; ClientSecret =:= undefined ->
  {error, mismatch};
authorize_client_credentials(ClientId, RedirectUri, ClientSecret, Scope) ->
pecypc_log:info({a, ClientId, RedirectUri, ClientSecret, Scope}),
  {ok, {client, ClientId}, Scope}.

%%
%% Register a new client.
%%
register_client(undefined, _, _, _) ->
  {error, badarg};
register_client(_, undefined, _, _) ->
  {error, badarg};
register_client(_, _, undefined, _) ->
  {error, scope};
register_client(_, _, _, undefined) ->
  {error, badarg};
register_client(Name, RedirectUri, Scope, {Secret, Ttl}) ->
  termit:encode_base64({client, Name, RedirectUri, Scope}, Secret, Ttl);
register_client(Name, RedirectUri, Scope, Secret) ->
  termit:encode_base64({client, Name, RedirectUri, Scope}, Secret).

%%
%% Get info on given client.
%%
validate_client(undefined, _) ->
  {error, badarg};
validate_client(_, undefined) ->
  {error, badarg};
validate_client(ClientId, Secret) ->
  termit:decode_base64(ClientId, Secret).

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
