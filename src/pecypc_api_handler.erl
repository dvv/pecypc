-module(pecypc_api_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-include("pecypc_api.hrl").

% -type state() :: tuple().
-type state() :: #state{}.
% % -opaque state() :: #state{}.
% -export_type([state/0]).
-type result() :: term().
-type error() :: term().

-callback authorize(State :: state()) ->
    {ok, State2 :: state()} |
    error.

-callback get(State :: state()) ->
    {ok, Result :: result()} |
    {goto, Where :: binary()} |
    error |
    {error, Reason :: error()}.

-callback put(State :: state()) ->
    ok |
    {ok, Result :: result()} |
    {goto, Where :: binary()} |
    error |
    {error, Reason :: error()}.

-callback delete(State :: state()) ->
    ok |
    accepted |
    error |
    {error, Reason :: error()}.
