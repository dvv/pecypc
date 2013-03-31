-module(pecypc_api_handler).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-include("pecypc_api.hrl").

-type query() :: proplist().
-type body() :: proplist().
-type options() :: proplist().
-type result() :: term().
-type error() :: term().

-callback get(Query :: query(), Options :: options()) ->
    {ok, Result :: result()} |
    {goto, Where :: binary()} |
    error |
    {error, Reason :: error()}.

-callback put(Body :: body(), Query :: query(), Options :: options()) ->
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

-callback handle(Function :: binary(), Args :: list(), Options :: options()) ->
    {ok, Result :: result()} |
    {error, Reason :: error()}.
