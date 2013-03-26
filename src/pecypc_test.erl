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

%%
%% Check access to resource.
%%
authorize(State = #state{auth = _Auth}) ->
pecypc_log:info({auth, State}),
  {ok, State}.

%%
%% Get resource. Returns raw term which will be serialized according to conneg.
%%
get(State = #state{options = Opts}) ->
pecypc_log:info({get, State}),
  {_, {Secret, _}} = lists:keyfind(security, 1, Opts),
  % {ok, [null, <<"GOT">>, 123]}.
  {ok, [{bearer, termit:encode_base64({user, <<"dvv">>}, Secret)}]}.

%%
%% Put or update resource, depending on method in State.
%% NB: In the future, all requests with body will be routed here.
%% May return raw term which will be serialized according to conneg.
%%
put(State) ->
pecypc_log:info({put, State}),
  % ok.
  % {ok, <<"PUT">>}.
  % {new, <<"foo">>}.
  {error, <<"PUT">>}.

%%
%% Start resource removal.
%% May return raw term which will be serialized according to conneg.
%%
delete(State) ->
pecypc_log:info({delete, State}),
  {error, [{foo, <<"bar">>}]}.
  % {error, bar}.
