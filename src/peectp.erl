%%
%% @doc Generic process registrar and operating primitives.
%%      Basically, gproc wrapper.
%%

-module(peectp).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

%%
%% -----------------------------------------------------------------------------
%% exports
%% -----------------------------------------------------------------------------
%%

-export([
    find_by_name/1,
    find_by_prop/1,
    await_for_name/1, await_for_name/2,
    await_for_prop/1, await_for_prop/2
  ]).

-export([
    send_by_name/2,
    send_by_prop/2,
    cast_by_name/2,
    cast_by_prop/2,
    call_by_name/2,
    call_by_prop/2
  ]).

-export([
    set_name/1,
    set_prop/1, set_prop/2,
    get_prop/1,
    unset_prop/1,
    set_value/1,
    get_value/0,
    unset_value/0,
    set_props/1,
    unset_props/1,
    unset_all/0
  ]).

%%
%% -----------------------------------------------------------------------------
%% Functions that find/create registered processes
%% -----------------------------------------------------------------------------
%%

%%
%% Return pid of process with unique Name.
%%

find_by_name(Name) ->
  gproc:where({n, l, {?MODULE, Name}}).

%%
%% Return list of pids of processes having property Prop.
%%

find_by_prop(Prop) ->
  gproc:lookup_pids({p, l, {?MODULE, Prop}}).

%%
%% Block forever until a process with unique Name is started.
%%

await_for_name(Name) ->
  gproc:await({n, l, {?MODULE, Name}}).

%%
%% Block for at most Timeout msec until a process with unique Name is started.
%%

await_for_name(Name, Timeout) ->
  gproc:await({n, l, {?MODULE, Name}}, Timeout).

%%
%% Block forever until a process having property Prop is started.
%%

await_for_prop(Prop) ->
  gproc:await({p, l, {?MODULE, Prop}}).

%%
%% Block for at most Timeout msec until a process having property Prop
%% is started.
%%

await_for_prop(Prop, Timeout) ->
  gproc:await({p, l, {?MODULE, Prop}}, Timeout).

%%
%% -----------------------------------------------------------------------------
%% Functions that operate on registered processes
%% -----------------------------------------------------------------------------
%%

%%
%% Send Msg to all processes tagged with Key.
%% Internal.
%%

send(Key, Msg) ->
  %gproc:bcast(Prop, Msg).
  gproc:send(Key, Msg).

%%
%% Send Msg to process with unique Name.
%%

send_by_name(Name, Msg) ->
  % NB: send/2 may throw if unique name is not registered
  try send({n, l, {?MODULE, Name}}, Msg) of
    Msg -> Msg
  catch _:_ ->
    Msg
  end.

%%
%% Send Msg to all processes having property Prop.
%%

send_by_prop(Prop, Msg) ->
  send({p, l, {?MODULE, Prop}}, Msg).

%%
%% Send Msg to all processes having property Prop except current process.
%%

send_to_others(Prop, Msg) ->
  lists:foreach(
      fun (P) -> P ! Msg end,
      find_by_prop(Prop) -- [self()]).

%%
%% Cast Msg to gen_server process with unique Name.
%%

cast_by_name(Name, Msg) ->
  case find_by_name(Name) of
    undefined -> ok;
    Pid -> gen_server:cast(Pid, Msg)
  end.

%%
%% Cast Msg to all gen_server processes having property Prop.
%%

cast_by_prop(Prop, Msg) ->
  lists:foreach(
      fun (Pid) -> gen_server:cast(Pid, Msg) end,
      find_by_prop(Prop)).

%%
%% Call gen_server process with unique Name with message Msg,
%% return reply.
%%

call_by_name(Name, Msg) ->
  case find_by_name(Name) of
    undefined -> {error, badarg};
    Pid -> gen_server:call(Pid, Msg)
  end.

%%
%% Call all gen_server processes having property Prop with message Msg,
%% return list of replies.
%%

call_by_prop(Prop, Msg) ->
  lists:map(
      fun (Pid) -> gen_server:call(Pid, Msg) end,
      find_by_prop(Prop)).

%%
%% -----------------------------------------------------------------------------
%% Functions to be called by registering process
%% -----------------------------------------------------------------------------
%%

%%
%% Tag with unique Name.
%%

set_name(Name) ->
  true = gproc:reg({n, l, {?MODULE, Name}}).

%%
%% Add a property Prop with no value.
%%

set_prop(Prop) ->
  true = gproc:reg({p, l, {?MODULE, Prop}}).

%%
%% Add a property Prop with Value.
%%

set_prop(Prop, Value) ->
  true = gproc:reg({p, l, {?MODULE, Prop}}, Value).

%%
%% Return the value of property Prop.
%%

get_prop(Prop) ->
  gproc:lookup_value({p, l, {?MODULE, Prop}}).

%%
%% Remove Prop and its value.
%%

unset_prop(Prop) ->
  true = gproc:unreg({p, l, {?MODULE, Prop}}).

%%
%% Associate a term Value with this process.
%%

set_value(Value) ->
  true = gproc:reg({p, l, {?MODULE, self()}}, Value).

%%
%% Return the term associated with this process with set_value/1.
%%

get_value() ->
  gproc:lookup_value({p, l, {?MODULE, self()}}).

%%
%% Remove the term associated with this process with set_value/1.
%%

unset_value() ->
  true = gproc:unreg({p, l, {?MODULE, self()}}).

%%
%% Add a bunch of properties given as [{Key, Value}].
%%

set_props([Prop | _H] = Props) when is_tuple(Prop) ->
  Props2 = [{{?MODULE, Key}, Value} || {Key, Value} <- Props],
  true = gproc:mreg(p, l, Props2);

%%
%% Add a bunch of properties given as [Key].
%%

set_props(Props) when is_list(Props) ->
  true = gproc:mreg(p, l, Props).

%%
%% Remove some of properties in one go.
%%

unset_props(Props) when is_list(Props) ->
  true = gproc:munreg(p, l, Props).

%%
%% Remove name and all properties.
%%

unset_all() ->
  gproc:goodbye().

%%
%% -----------------------------------------------------------------------------
%% Tests @todo learn how to do :)
%% -----------------------------------------------------------------------------
%%
