-module(pecypc_session).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

%% -----------------------------------------------------------------------------
%% API exports
%% -----------------------------------------------------------------------------

-export([start_link/1]).

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

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% -----------------------------------------------------------------------------
%% gen_server behaviour
%% -----------------------------------------------------------------------------

-record(state, {
  }).

init(_Arg) ->
  pecypc_log:info({init_pecypc_session, _Arg}),
  process_flag(trap_exit, true), % activate terminate callback
  State = #state{},
  {ok, State}.

terminate(Reason, State) ->
  pecypc_log:info({terminate_pecypc_session, Reason, State}),
  ok.

code_change(OldVsn, State, Extra) ->
  pecypc_log:info({code_change_pecypc_session, OldVsn, State, Extra}),
  {ok, State}.

handle_call(Request, From, State) ->
  pecypc_log:info({call_pecypc_session, Request, From, State}),
  {reply, ok, State}.

handle_cast(stop, State) ->
  pecypc_log:info({cast_pecypc_session, stop, State}),
  {stop, normal, State};

handle_cast(Msg, State) ->
  pecypc_log:info({cast_pecypc_session, Msg, State}),
  {noreply, State}.

handle_info(Info, State) ->
  pecypc_log:info({info_pecypc_session, Info, State}),
  {noreply, State}.

%% -----------------------------------------------------------------------------
%% Private functions
%% -----------------------------------------------------------------------------

stop() ->
  gen_server:cast(?MODULE, stop).
