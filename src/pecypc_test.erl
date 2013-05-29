-module(pecypc_test).

-behaviour(cowboy_resource_handler).
-export([
    init/3,
    allowed/2,
    authorize/3,
    call/3,
    create/3,
    delete/2,
    get/2,
    put/3,
    update/3
  ]).

init(_Transport, _Req, _Options) ->
  {upgrade, protocol, cowboy_resource}.

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
allowed(update, _) ->
  false;
allowed(Method, {_Identity, Scope}) ->
pecypc_log:info({allowed, Method, Scope}),
  true.
  % BaseScope = <<"you-are-admin">>,
  % oauth2_priv_set:is_member([BaseScope, Method], oauth2_priv_set:new(Scope)).

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  {ok, [{<<"x">>, <<"y">>}]}.

create(B, Q, O) ->
pecypc_log:info({create, B, Q, O}),
  {ok, B}.

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

update(B, Q, O) ->
pecypc_log:info({update, B, Q, O}),
  {ok, <<"UPDATED">>}.
  % {error, <<"PUT">>}.

delete(Q, O) ->
pecypc_log:info({delete, Q, O}),
  {error, [{foo, <<"bar">>}]}.

call(<<"add">>, [X, Y], _O) when is_number(X) ->
pecypc_log:info({add, X, Y}),
  {ok, X + Y}.

%%
%% -----------------------------------------------------------------------------
%% Validation rules.
%% -----------------------------------------------------------------------------
%%

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
  {<<"l">>, required, {list, [{any}]}},
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
