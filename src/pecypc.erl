-module(pecypc).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

%% -----------------------------------------------------------------------------
%% API exports
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server exports
%% -----------------------------------------------------------------------------

-behaviour(gen_server).
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
  ]).

%% -----------------------------------------------------------------------------
%% API functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server behaviour
%% -----------------------------------------------------------------------------

-record(state, {
    db
  }).

init(_Arg) ->
  pecypc_log:info({init_pecypc, _Arg}),
  process_flag(trap_exit, true), % activate terminate callback
  {ok, C} = pgsql:connect("127.0.0.1", "postgres", "postgres", [{database, "hypo"}]),
  State = #state{
    db = C
  },
  {ok, State}.

terminate(Reason, State) ->
  pecypc_log:info({terminate_pecypc, Reason, State}),
  pgsql:close(State#state.db),
  ok.

code_change(OldVsn, State, Extra) ->
  pecypc_log:info({code_change_pecypc, OldVsn, State, Extra}),
  {ok, State}.

handle_call({db, index} = Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "select id, data from cons",
      [])
  of
    {ok, _, Rows} ->
      {ok, [[{id, Id}, {data, jsx:decode(Data)}] || {Id, Data} <- Rows]};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, index, Ids} = Request, From, State) when is_list(Ids) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "select id, data from cons where id = any($1)",
      [[list_to_integer(binary_to_list(Id)) || Id <- Ids]])
  of
    {ok, _, Rows} ->
      {ok, [[{id, Id}, {data, jsx:decode(Data)}] || {Id, Data} <- Rows]};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, get, Id} = Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "select data from cons where id = $1",
      [list_to_integer(binary_to_list(Id))])
  of
    {ok, _, []} ->
      {error, enoent};
    {ok, _, [{Data}]} ->
      {ok, jsx:decode(Data)};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, add, Data} = Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "insert into cons (data) values($1) returning id",
      [jsx:encode(Data)])
  of
    {ok, 1, _, [{Id}]} ->
      {ok, list_to_binary(integer_to_list(Id))};
    {ok, _, []} ->
      {error, enoent};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, put, Ids, Data} = Request, From, State) when is_list(Ids) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "update cons set data = $2 where id = any($1)",
      [[list_to_integer(binary_to_list(Id)) || Id <- Ids], jsx:encode(Data)])
  of
    {ok, _} ->
      ok;
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, put, Id, Data} = Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "update cons set data = $2 where id = $1",
      [list_to_integer(binary_to_list(Id)), jsx:encode(Data)])
  of
    {ok, 1} ->
      ok;
    {ok, 0} ->
      {error, enoent};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, remove, Ids} = Request, From, State) when is_list(Ids) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "delete from cons where id = any($1)",
      [[list_to_integer(binary_to_list(Id)) || Id <- Ids]])
  of
    {ok, _} ->
      ok;
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call({db, remove, Id} = Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  Reply = case pgsql:equery(State#state.db,
      "delete from cons where id = $1",
      [list_to_integer(binary_to_list(Id))])
  of
    {ok, 1} ->
      ok;
    {ok, 0} ->
      {error, enoent};
    {error, _} ->
      error
  end,
  {reply, Reply, State};

handle_call(Request, From, State) ->
  pecypc_log:info({call_pecypc, Request, From, State}),
  {reply, ok, State}.

handle_cast(stop, State) ->
  pecypc_log:info({cast_pecypc, stop, State}),
  {stop, normal, State};

handle_cast(Msg, State) ->
  pecypc_log:info({cast_pecypc, Msg, State}),
  {noreply, State}.

handle_info(Info, State) ->
  pecypc_log:info({info_pecypc, Info, State}),
  {noreply, State}.

%% -----------------------------------------------------------------------------
%% Private functions
%% -----------------------------------------------------------------------------

stop() ->
  gen_server:cast(?MODULE, stop).

