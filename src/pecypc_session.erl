-module(pecypc_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-include_lib("stdlib/include/qlc.hrl").

%% -----------------------------------------------------------------------------
%% API exports
%% -----------------------------------------------------------------------------

-export([
    add/1,
    add/2,
    index/0,
    index/1,
    kill/0,
    kill/1,
    remove/1,
    start_link/1,
    where/1
  ]).

-export([
    call/1,
    call/2,
    read/0,
    read/1,
    touch/0,
    touch/1,
    write/2
  ]).

-export([
    init/2,
    handle/2
  ]).

-define(KEY(Id), {n, l, {?MODULE, Id}}).
-define(PID(Id), {via, gproc, ?KEY(Id)}).

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

start_link(Arg = {Id, _Opts}) ->
  gen_server:start_link({via, gproc, {n, l, {?MODULE, Id}}}, ?MODULE, Arg, []).

add(Data) ->
  add(base64url:encode(crypto:rand_bytes(16)), Data).

add(Id, Data) ->
  case supervisor:start_child(pecypc_session_sup, [{Id, Data}]) of
    {ok, _Pid} ->
      {ok, Id};
    {error, {already_started, _Pid}} ->
      {ok, Id}
  end.

remove(Id) ->
  gen_server:cast(?PID(Id), stop).

where(Id) ->
  gproc:where(?KEY(Id)).

all() ->
  gproc:select([{{?KEY('_'), '$1', '_'}, [], ['$1']}]).

others() ->
  all() -- [self()].

all_ids() ->
  gproc:select([{{?KEY('$1'), '_', '_'}, [], ['$1']}]).

index() ->
  gproc:select([{{?KEY('$1'), '$2', '_'}, [], [{{'$1','$2'}}]}]).

index(Fun) when is_function(Fun) ->
  % @todo: next_answers defaults to 10 records
  qlc:next_answers(qlc:cursor(qlc:q([
    {Id, Pid} || {?KEY(Id), Pid, Value} <- gproc:table(),
      Fun(Value)
  ])), all_remaining).

call(Cmd) ->
  [gen_server:call(P, {command, Cmd}) || P <- all()].

call(Id, Cmd) ->
  gen_server:call(?PID(Id), {command, Cmd}).

read() ->
  gproc:select([{{?KEY('_'), '_', '$1'}, [], ['$1']}]).

read(L) when is_list(L) ->
  [read(Id) || Id <- L];
read(Id) ->
  gproc:select([{{?KEY(Id), '_', '$1'}, [], ['$1']}]).

write(Id, Data) ->
  gen_server:cast(?PID(Id), {set, Data}).

touch() ->
  [gen_server:cast(P, touch) || P <- all()],
  ok.

touch(Id) ->
  gen_server:cast(?PID(Id), touch).

kill() ->
  kill(normal).

kill(Reason) ->
  [exit(P, Reason) || P <- all()],
  ok.

%% -----------------------------------------------------------------------------
%% gen_server behaviour
%% -----------------------------------------------------------------------------

-record(state, {
    expires,
    id,
    state
  }).

init({Id, Opts}) ->
  pecypc_log:info({init_pecypc_session, Id, Opts}),
  S = init(Id, Opts),
  true = gproc:set_value(?KEY(Id), S),
  % true = gproc:mreg(p, l, [{{?MODULE, Key}, Value} || {Key, Value} <- Props]),
  {MegaSecs, Secs, _} = os:timestamp(),
  State = #state{
    expires = MegaSecs * 1000000 + Secs,
    id = Id,
    state = S
  },
  {ok, prolong(State)}.

terminate(Reason, State) ->
  pecypc_log:info({terminate_pecypc_session, Reason, State}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(get, _From, State = #state{state = S}) ->
  {reply, S, prolong(State)};
handle_call({command, Cmd}, _From, State = #state{id = Id, state = S}) ->
  % pecypc_log:info({call_pecypc_session_command, Cmd, State}),
  {Reply, S2} = handle(Cmd, S),
  gproc:set_value(?KEY(Id), S2),
  {reply, Reply, prolong(State#state{state = S2})};
handle_call(_Request, _From, State) ->
  % pecypc_log:info({call_pecypc_session, Request, From, State}),
  {reply, {error, badarg}, State}.

handle_cast({set, S}, State = #state{id = Id}) ->
  gproc:set_value(?KEY(Id), S),
  {noreply, prolong(State#state{state = S})};
handle_cast(touch, State) ->
  {noreply, prolong(State)};
handle_cast(stop, State = #state{state = S}) ->
  handle(stop, S),
  {stop, normal, State};
handle_cast(Msg, State = #state{id = Id, state = S}) ->
  % pecypc_log:info({cast_pecypc_session, Msg, State}),
  {ok, S2} = handle(Msg, S),
  gproc:set_value(?KEY(Id), S2),
  {noreply, prolong(State#state{state = S2})}.

handle_info({'EXIT', _, Reason}, State) ->
  terminate(Reason, State),
  {noreply, State};
handle_info(info, State) ->
  pecypc_log:info({info_pecypc_session, info, State}),
  {noreply, State}.

%% -----------------------------------------------------------------------------
%% Private functions
%% -----------------------------------------------------------------------------

prolong(State = #state{expires = Expires}) ->
  Expires2 = Expires + 120,
  State#state{expires = Expires2}.

init(_Id, Opts) ->
  [{opts, Opts}].

handle(_Msg, State) ->
  {ok, State}.
