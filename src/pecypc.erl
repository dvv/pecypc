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
  }).

init(_Arg) ->
  pecypc_log:info({init_pecypc, _Arg}),
  process_flag(trap_exit, true), % activate terminate callback
  State = #state{},
  {ok, State}.

terminate(Reason, State) ->
  pecypc_log:info({terminate_pecypc, Reason, State}),
  ok.

code_change(OldVsn, State, Extra) ->
  pecypc_log:info({code_change_pecypc, OldVsn, State, Extra}),
  {ok, State}.

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

