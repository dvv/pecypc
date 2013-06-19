-module(pecypc_session_sup).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    % sessions
    {undefined, {pecypc_session, start_link, []}, transient, 5000,
        worker, [pecypc_session]}
  ]}}.
