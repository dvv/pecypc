-module(pecypc_test).

-behaviour(pecypc_api_handler).
-export([
    authorize/1,
    get/2,
    put/2,
    delete/2
  ]).

% backend
-behaviour(saddle_backend).
-export([
    authorize_client_credentials/4,
    authorize_username_password/3
  ]).

-export([handle/2]).

-include("pecypc_api.hrl").

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

handle(<<"get">>, [Q, O]) ->
  get(Q, O);
handle(<<"add">>, [X, Y]) ->
pecypc_log:info({add, X, Y}),
  {ok, X + Y}.

authorize(State = #state{auth = _Auth, params = Params}) ->
pecypc_log:info({auth, State}),
  case lists:keyfind(bucket, 1, Params) of
    {_, <<"dvv">>} ->
      {ok, State};
    _ ->
      error
  end.

get(Query, Opts) ->
pecypc_log:info({get, Query, Opts}),
  {ok, <<"GOT">>}.

%% @todo distinguish POST -> RPC, PUT -> new, PATCH -> update
put(Body, Opts) ->
pecypc_log:info({put, Body, Opts}),
  % ok.
  % {ok, <<"PUT">>}.
  % {new, <<"foo">>}.
  % {error, <<"PUT">>}.
  {_, {Secret, _}} = lists:keyfind(security, 1, Opts),
  {ok, [{bearer, termit:encode_base64([
      {user, <<"dvv">>},
      {acl, [
        <<"api">>, <<"dvv">>
      ]}
    ], Secret)}]}.

delete(Query, Opts) ->
pecypc_log:info({delete, Query, Opts}),
  {error, [{foo, <<"bar">>}]}.
  % {error, bar}.

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
