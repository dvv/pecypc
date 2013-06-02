-module(pecypc_hypo).

%-behaviour(cowboy_http_handler).
-export([
    init/3
  ]).

-behaviour(cowboy_resource_handler).
-export([
    allowed/2,
    authorize/3,
    call/3,
    delete/2,
    get/2,
    patch/3,
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

authorize(_Type, _Credentials, _Options) ->
  {ok, {<<"foo">>, <<"you-are-admin.*">>}}.

% authorize(password, {<<"admin">>, <<"admin">>}, _Options) ->
% % authorization: 'basic YWRtaW46YWRtaW4='
%   {ok, {<<"foo">>, <<"you-are-admin.*">>}};
% authorize(_, _, _) ->
%   {error, eaccess}.

allowed(_Method, _Auth) ->
  true.

get(Q, O) ->
pecypc_log:info({get, Q, O}),
  case lists:keyfind(id, 1, Q) of
    {_, Id} ->
      case binary:split(Id, <<",">>, [global]) of
        [Exact] ->
          gen_server:call(pecypc, {db, get, Exact});
        Set ->
          gen_server:call(pecypc, {db, index, Set})
      end;
    false ->
      gen_server:call(pecypc, {db, index})
  end.

post(B, Q, O) ->
pecypc_log:info({post, B, Q, O}),
  case lists:keyfind(id, 1, Q) of
    {_, _} ->
      {error, forbidden};
    false ->
      case gen_server:call(pecypc, {db, add, B}) of
        {ok, Id} ->
          {goto, Id};
        Error ->
          Error
      end
  end.

put(B, Q, O) ->
pecypc_log:info({put, B, Q, O}),
  case lists:keyfind(id, 1, Q) of
    {_, Id} ->
      case binary:split(Id, <<",">>, [global]) of
        [Exact] ->
          gen_server:call(pecypc, {db, put, Exact, B});
        Set ->
          gen_server:call(pecypc, {db, put, Set, B})
      end;
    false ->
      {error, forbidden}
  end.

patch(B, Q, O) ->
pecypc_log:info({patch, B, Q, O}),
  {error, forbidden}.

delete(Q, O) ->
pecypc_log:info({delete, Q, O}),
  case lists:keyfind(id, 1, Q) of
    {_, Id} ->
      gen_server:call(pecypc, {db, remove, Id});
    false ->
      {error, forbidden}
  end.

call(<<"add">>, [X, Y], _O) when is_number(X) ->
pecypc_log:info({add, X, Y}),
  {ok, X + Y}.
