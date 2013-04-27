-module(pecypc_auth).
-behaviour(cowboy_middleware).

-export([
    execute/2
  ]).

%%
%% The idea is to switch environment dispatch rules based on Authorization.
%%
execute(Req, Env) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {User, _}}, Req2} ->
      case lists:keyfind(User, 1, dispatch()) of
        false ->
          {error, 404, Req2};
        {_, Dispatch} ->
          {ok, Req2, lists:keyreplace(dispatch, 1, Env, {dispatch, Dispatch})}
      end;
    _ ->
      {error, 401, cowboy_req:set_resp_header(<<"www-authenticate">>, <<"basic">>, Req)}
  end.

dispatch() -> [
  {<<"u">>, cowboy_router:compile([{'_', lists:flatten(routes_for_user(<<"u">>))}])},
  {<<"a">>, cowboy_router:compile([{'_', lists:flatten(routes_for_admin(<<"a">>))}])}
].

routes_for_guest() -> [
  % static content: /* -> /priv/www/*
  {"/[...]", cowboy_static, [
    %{directory, "priv/www"},
    {directory, {priv_dir, pecypc, [<<"www">>]}},
    % @todo get rid of gen_server-ish mimetypes
    {mimetypes, {{mimetypes, path_to_mimes}, default}}
  ]}
].

routes_for_user(<<"u">>) -> [
  {"/u", handler, []},
  routes_for_guest()
];
routes_for_user(_) ->
  [].

routes_for_admin(<<"a">>) -> [
  {"/a", handler, []},
  routes_for_user(<<"u">>)
];
routes_for_admin(_) ->
  [].
