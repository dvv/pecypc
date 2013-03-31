-module(pecypc_user).

-behaviour(pecypc_api_handler).
-export([
    delete/2,
    get/2,
    put/3
  ]).

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  D = [{<<"email">>, <<"1@2.3">>}, {<<"foo">>, [{<<"bar">>, 1}, {<<"baz">>, 2}]}],
  validate(schema(get), D).

put(D, Q, O) ->
pecypc_log:info({put, D, Q, O}),
  case lists:keyfind(method, 1, O) of
    {_, <<"PUT">>} ->
      validate(schema(put), D);
    {_, <<"PATCH">>} ->
      validate(schema(set), D)
  end.

delete(Q, O) ->
pecypc_log:info({delete, Q, O}),
  ok.

validate(Schema, D) ->
  % case jiffy_v:validate(Schema, D, fun validator/3) of
  case jiffy_v:validate(Schema, D) of
    {[], Validated} ->
      {ok, Validated};
    {Errors, _Partial} ->
      % {error, [{error, [{C, P} || {C, P, _} <- Errors]}]}
      {error, [{P, C} || {C, P, _} <- Errors]}
  end.

% validator(validate, _, _) -> {ok, valid};
% validator(fix, _, _) -> {ok, <<"FIXED">>}.

schema(get) -> {hash, [
  % {<<"email">>, required, {string}},
  {<<"foo">>, required, {hash, [
    {<<"baz">>, required, {any}}
  ]}}
]};
schema(put) -> {hash, [
  {<<"email">>, required, {string}},
  {<<"foo">>, required, {hash, [
    {<<"bar">>, required, {any}}
  ]}}
]};
schema(set) -> {hash, [
  {<<"email">>, optional, {string}},
  {<<"foo">>, optional, {hash, [
    {<<"bar">>, optional, {any}}
  ]}}
]}.
