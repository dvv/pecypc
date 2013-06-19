-module(pecypc_sess).

-behaviour(cowboy_resource_handler).
-export([
    init/3,
    allowed/2,
    authorize/3,
    delete/2,
    get/2,
    post/3,
    put/3
  ]).

init(_Transport, _Req, _Options) ->
  {upgrade, protocol, cowboy_resource}.

%%
%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------
%%

authorize(Type, Credentials, _Options) ->
  {ok, {<<"sess">>, <<"you-are-admin.*">>}}.

allowed(_, _) ->
  true.

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  case lists:keyfind(id, 1, Q) of
    false ->
      {error, enoent};
    {_, Sid} ->
      case pecypc_session:read(Sid) of
        undefined ->
          {error, enoent};
        S ->
          {ok, S}
      end
  end.

post(B, Q, O) ->
pecypc_log:info({post, B, Q, O}),
  {ok, Sid} = pecypc_session:add(B),
  {goto, Sid}.

put(B, Q, O) ->
pecypc_log:info({put, B, Q, O}),
  {_, Sid} = lists:keyfind(id, 1, Q),
  X = pecypc_session:read(Sid),
  X2 = B,
  pecypc_session:write(Sid, X2),
  ok.

delete(Q, O) ->
pecypc_log:info({delete, Q, O}),
  {_, Sid} = lists:keyfind(id, 1, Q),
  ok = pecypc_session:remove(Sid),
  accepted.
