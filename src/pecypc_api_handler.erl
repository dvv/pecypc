-module(pecypc_api_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-include("pecypc_api.hrl").

-type proplist() :: list({term(), term()}).

% -type state() :: tuple().
-type state() :: #state{}.
% % -opaque state() :: #state{}.
% -export_type([state/0]).
-type query() :: proplist().
-type body() :: proplist().
-type options() :: proplist().
-type result() :: term().
-type error() :: term().

-callback authorize(State :: state()) ->
    {ok, State2 :: state()} |
    error.

-callback get(Query :: query(), Options :: options()) ->
    {ok, Result :: result()} |
    {goto, Where :: binary()} |
    error |
    {error, Reason :: error()}.

-callback put(Body :: body(), QueryAndOptions :: options()) ->
    ok |
    {ok, Result :: result()} |
    {goto, Where :: binary()} |
    error |
    {error, Reason :: error()}.

-callback delete(Query :: query(), Options :: options()) ->
    ok |
    accepted |
    error |
    {error, Reason :: error()}.

-callback handle(Function :: binary(), Arguments :: list()) ->
    {ok, Result :: result()} |
    {error, Reason :: error()}.
