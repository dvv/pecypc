-module(pecypc_sup).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(supervisor).
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Helper macros for declaring children of supervisor
%% -----------------------------------------------------------------------------

-define(SERVER(Name), {
    Name, {gen_server, start_link, [{local, Name}, Name, [], []]},
    permanent, 5000, worker, [Name]
  }).

-define(FSM(Name), {
    Name, {gen_fsm, start_link, [{local, Name}, Name, [], []]},
    permanent, 5000, worker, [Name]
  }).

-define(EVENT(Name), {
    Name, {gen_event, start_link, [{local, Name}, Name, [], []]},
    permanent, 5000, worker, [dynamic]
  }).

-define(SUPER(Name), {
    Name, {Name, start_link, []},
    permanent, infinity, supervisor, [Name]
  }).

-define(WORKER(Name), {
    Name, {Name, start_link, []},
    permanent, 5000, worker, [Name]
  }).

%% -----------------------------------------------------------------------------
%% Supervisor is starting
%% -----------------------------------------------------------------------------

init([]) ->
  % return the list of supervised children
  {ok, { {one_for_one, 5, 10}, [
    % main logic
    ?SERVER(pecypc),
    % sessions
    ?SUPER(pecypc_session_sup),
    % web server
    ?WORKER(pecypc_web)
  ]} }.
