-module(pecypc_session_sup).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
-export([start_child/0, start_child/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    % sessions
    {undefined, {pecypc_session, start_link, []}, temporary, 5000,
        worker, [pecypc_session]}
  ]}}.

start_child() ->
  start_child([]).

start_child(Arg) ->
  supervisor:start_child(?MODULE, [Arg]).
