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
    authorize_username_password/3
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
  {_, {Secret, _}} = lists:keyfind(security, 1, O),
  {ok, [{bearer, termit:encode_base64({{user, <<"dvv">>}, [
        <<"admin.add">>, <<"admin.PUT">>, <<"admin.GET">>, <<"user.*">>
      ]}, Secret)}]}.

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
