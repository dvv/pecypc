-module(pecypc_test).
-behaviour(pecypc_api_handler).

-export([
    authorize/1,
    get/1,
    put/1,
    delete/1
  ]).

-include("pecypc_api.hrl").

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

authorize(State = #state{auth = _Auth, params = Params}) ->
pecypc_log:info({auth, State}),
  case lists:keyfind(bucket, 1, Params) of
    {_, <<"dvv">>} ->
      {ok, State};
    _ ->
      error
  end.

get(State = #state{options = Opts}) ->
pecypc_log:info({get, State}),
  {_, {Secret, _}} = lists:keyfind(security, 1, Opts),
  % {ok, [null, <<"GOT">>, 123]}.
  {ok, [{bearer, termit:encode_base64({user, <<"dvv">>}, Secret)}]}.

put(State = #state{options = Opts}) ->
pecypc_log:info({put, State}),
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

delete(State) ->
pecypc_log:info({delete, State}),
  {error, [{foo, <<"bar">>}]}.
  % {error, bar}.
