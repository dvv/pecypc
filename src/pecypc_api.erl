%%% ----------------------------------------------------------------------------
%%%
%%% @doc Skeleton for a HTTP resource.
%%%
%%% ----------------------------------------------------------------------------

-module(pecypc_api).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

% -behaviour(cowboy_rest_handler).
-export([
    init/3,
    terminate/3,
    rest_init/2,
    resource_available/2,
    allowed_methods/2,
    malformed_request/2,
    is_authorized/2,
    forbidden/2,
    valid_content_headers/2,
    options/2,
    content_types_accepted/2,
    content_types_provided/2,
    % post_is_create/2,
    % created_path/2,
    process_post/2,
    delete_resource/2,
    delete_completed/2
  ]).

% getters
-export([
    get_resource/2
  ]).

% setters
-export([
    put_form/2,
    put_json/2
  ]).

-include("pecypc_api.hrl").

init(_Transport, Req, Opts) ->
  Req2 = case lists:keyfind(patch, 1, Opts) of
    {_, true} ->
      % apply apigen pragmatic REST recommendations
      cowboy_patch:patch_pragmatic_rest(
           cowboy_patch:patch_method(
           cowboy_patch:patch_headers(Req)));
    false ->
      Req
  end,
  % extract request info
  {Params, Req3} = cowboy_req:bindings(Req2),
  {Query, Req4} = cowboy_req:qs_vals(Req3),
  %lists:ukeymerge(1, lists:ukeysort(1, Params), lists:ukeysort(1, Query)),
  {Method, Req5} = cowboy_req:method(Req4),
  % extract options
  {_, Handler} = lists:keyfind(handler, 1, Opts),
  % go rest
  {upgrade, protocol, cowboy_rest, Req5, #state{
      method = Method,
      params = Params,
      query = Query,
      options = Opts,
      handler = Handler
    }}.

terminate(_Reason, _Req, _State) ->
  ok.

rest_init(Req, State) ->
  {ok, Req, State}.

resource_available(Req, State) ->
  {true, Req, State}.

allowed_methods(Req, State = #state{options = Opts}) ->
  case lists:keyfind(allow, 1, Opts) of
    {_, Methods} ->
      {Methods, Req, State};
    false ->
      {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
          <<"PATCH">>, <<"HEAD">>], Req, State}
  end.

%%
%% Validate GET requests. Body is not yet available and conneg is not yet done.
%%
malformed_request(Req, State) ->
  {false, Req, State}.

%%
%% Validate authentication credentials provided and not forged.
%% Bearer or basic authorization required.
%% @todo consider dropping security key before continuing
%%
is_authorized(Req, State = #state{options = Opts}) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"bearer">>, Token}, Req2} ->
      % NB: dvv/termit for opaque signed encoded data
      {_, {Secret, TimeToLive}} = lists:keyfind(security, 1, Opts),
      case termit:decode_base64(Token, Secret, TimeToLive) of
        {ok, Auth} ->
          {true, Req2, State#state{auth = Auth}};
        {error, _Reason} ->
          {{false, <<"Bearer, Basic">>}, Req2, State}
      end;
    {ok, {<<"basic">>, Credentials}, Req2} ->
      {true, Req2, State#state{auth = Credentials}};
    _ ->
      {{false, <<"Bearer, Basic">>}, Req, State}
  end.

%%
%% Checks user is authorized to access the resource.
%% NB: POST carries batch RPC and access will be checked later on
%%   for each request in the batch.
%%
forbidden(Req, State = #state{method = <<"POST">>}) ->
  % {false, cowboy_req:set([{method, <<"PUT">>}], Req), State};
  {false, Req, State};
%%
%% Other methods mean single action and access can be checked at once.
%%
forbidden(Req, State = #state{method = Method, auth = Auth, options = Opts}) ->
  {_, BaseScope} = lists:keyfind(scope, 1, Opts),
  {not call_allowed(Method, BaseScope, Auth), Req, State}.

%%
%% This may be used to protect against forged Content-Type: headers.
%% E.g. AFAICS <<"application/json, text/html">> chokes cowboy_rest conneg.
%%
valid_content_headers(Req, State) ->
  {true, Req, State}.

%%
%% Called on OPTIONS. Use to set custom headers. Note returned tag is 'ok'.
%%
options(Req, State) ->
  {ok, Req, State}.

%%
%% Enumerate content types resource may process.
%%
content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, put_json},
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, put_form}
  ], Req, State}.

%%
%% Enumerate content types resource may return.
%%
content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, get_resource},
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, get_resource}
  ], Req, State}.

%%
%% -----------------------------------------------------------------------------
%% Resource operations.
%% -----------------------------------------------------------------------------
%%

%%
%% Delegates actual processing to application's get/2 handler.
%% Encodes response entity.
%%
%%
%% - {Body, Req, State} --> 200 OK
%% - {halt, Req, State} --> no further processing
%%
get_resource(Req, State = #state{
    query = Query, handler = Handler, options = Opts}) ->
  try Handler:get(Query, Opts) of
    {ok, Result} ->
      {serialize(Result, Req), Req, State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State};
    {goto, Location} ->
      {halt, cowboy_req:set_resp_header(<<"location">>, Location, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in get/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end.

%%
%% Route POST to put_*.
%%
% post_is_create(Req, State) ->
%   {true, Req, State}.

% created_path(Req, State) ->
%   {<<>>, Req, State}.

process_post(Req, State) ->
  case put_json(Req, State) of
    % mimic PUT's behaviour on false response
    {false, Req2, State2} ->
      {ok, Req3} = cowboy_req:reply(422, Req2),
      {halt, Req3, State2};
    Else ->
      Else
  end.

%%
%% Handle PUT/PATCH requests.
%%
%% - {false, Req, State} --> 422 Unprocessable Entity
%% - {true, Req, State} --> 204 No Content
%% - {halt, Req, State} --> no further processing
%%
%% The following applies only on PUT
%% - set response location: and {true, Req, State} --> 201 Created
%% - set response body and {true, Req, State} --> 200 OK
%%
put_json(Req, State) ->
  % @todo make it streaming
  {ok, Body, Req2} = cowboy_req:body(Req),
  case jsx:decode(Body, [{error_handler, fun(_, _, _) -> {error, badarg} end}])
  of
    {error, _} ->
      {false, Req2, State};
    {incomplete, _} ->
      {false, Req2, State};
    Data ->
      put_resource(Req2, State#state{body = Data})
  end.

put_form(Req, State) ->
  {ok, Result, Req2} = cowboy_req:body_qs(Req),
  put_resource(Req2, State#state{body = Result}).

%%
%% @todo add more body decoders here. multipart is welcome.
%%

%%
%% POST is used for free-style RPC.
%% Take batch of requests from body, returns batch of responses.
%% Requests processing delegated to application's handle(Method, [Args]).
%%
%% Request is triplet array: [Method, Params, Id].
%% Response is triplet array: [null, Result, Id] | [Error, null, Id].
%%
put_resource(Req, State = #state{method = <<"POST">>}) ->
  {true, set_resp_body(batch_rpc(State), Req), State};

%%
%% Delegates actual processing to application's put/3 handler.
%% Encodes possible response entity.
%%
put_resource(Req, State = #state{method = Method,
    query = Query, body = Data, handler = Handler, options = Opts}) ->
  % try apply(Handler,
  %           binary_to_atom(cowboy_bstr:to_lower(Method), latin1),
  %           [Data, Query, Opts])
  % of
  try Handler:put(Data, Query, [{method, Method} | Opts]) of
    {ok, Body} ->
      {true, set_resp_body(Body, Req), State};
    ok ->
      {true, Req, State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State};
    {goto, Location} ->
      {true, cowboy_req:set_resp_header(<<"location">>, Location, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in put/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end.

%%
%% Delegates actual processing to application's delete/2 handler.
%%
%%
%% NB: must be defined if DELETE in allowed methods or 500 Internal Server Error
%% It should start deleting the resource and return.
%% - {true, Req, State} --> 204 No Content, unless delete_completed/2 defined
%% - {X =/= true, Req, State} --> 500 Internal Server Error
%% - {halt, Req, State} --> no further processing
%%
delete_resource(Req, State = #state{
    query = Query, handler = Handler, options = Opts}) ->
  try Handler:delete(Query, Opts) of
    ok ->
      {true, Req, State#state{completed = true}};
    accepted ->
      {true, Req, State#state{completed = false}};
    error ->
      {halt, respond(400, undefined, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State}
  catch Class:Reason ->
    error_logger:error_msg(
      "** API handler ~p terminating in delete/3~n"
      "   for the reason ~p:~p~n** State was ~p~n"
      "** Stacktrace: ~p~n~n",
      [Handler, Class, Reason, State, erlang:get_stacktrace()]),
    {halt, respond(500, Reason, Req), State}
  end.

%%
%% It indicates whether the resource has been deleted yet.
%% - {true, Req, State} --> go ahead with 200/204
%% - {false, Req, State} --> 202 Accepted
%% - {halt, Req, State} --> no further processing
%%
delete_completed(Req, State = #state{completed = Completed}) ->
  {Completed, Req, State}.

%%
%%------------------------------------------------------------------------------
%% RPC functions
%%------------------------------------------------------------------------------
%%

batch_rpc(#state{body = Batch,
    handler = Handler, auth = Auth, options = Opts}) ->
  {_, BaseScope} = lists:keyfind(scope, 1, Opts),
  [case call_allowed(Method, BaseScope, Auth) of
    true ->
      try Handler:handle(Method, Args, Opts) of
        {ok, Result} ->
          [null, Result, Id];
        ok ->
          [null, null, Id];
        % {error, Reason} when is_atom(Reason) ->
        %   [atom_to_binary(Reason, latin1), null, Id];
        {error, Reason} ->
          [Reason, null, Id];
        error ->
          [reason(undefined), null, Id];
        accepted ->
          [null, null, Id];
        {goto, Location} ->
          [null, Location, Id]
      catch
        _:function_clause ->
          [<<"enoent">>, null, Id];
        _:badarg ->
          [<<"einval">>, null, Id];
        _:badarith ->
          [<<"einval">>, null, Id];
        Class:Reason ->
          error_logger:error_msg(
            "** API RPC handler ~p terminating in handle/3~n"
            "   for the reason ~p:~p~n** Method was ~p~n"
            "** Arguments were ~p~n** Stacktrace: ~p~n~n",
            [Handler, Class, Reason, Method, Args, erlang:get_stacktrace()]),
          [<<"einval">>, null, Id]
      end;
    false ->
      [<<"eperm">>, null, Id]
  end || [Method, Args, Id] <- Batch].

%%
%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------
%%

%%
%% Return true if Method of BaseScope is in AllowedScope, according to
%% https://github.com/kivra/oauth2#scope.
%%
%% NB: Authentication token hardcoded to be tuple {Identity, AllowedScope}.
%% @todo reconsider, maybe kick out to separate module or delegate to resource.
%%
call_allowed(Method, BaseScope, {_Identity, AllowedScope}) ->
% pecypc_log:info({auth,
%     << BaseScope/binary, $., Method/binary >>, AllowedScope}),
  oauth2_priv_set:is_member(<< BaseScope/binary, $., Method/binary >>,
      oauth2_priv_set:new(AllowedScope)).

%%
%% Error reporting.
%%
reason(undefined) ->
  reason(<<"unknown">>);
reason(Reason) when is_list(Reason) ->
  Reason;
reason(Reason) when is_binary(Reason); is_number(Reason) ->
  reason([{error, Reason}]);
reason(Reason) when is_atom(Reason) ->
  reason(atom_to_binary(Reason, latin1)).

%%
%% Response helpers
%%
respond(Status, Reason, Req) ->
  {ok, Req2} = cowboy_req:reply(Status, set_resp_body(reason(Reason), Req)),
  Req2.

set_resp_body(Body, Req) ->
  cowboy_req:set_resp_body(serialize(reason(Body), Req), Req).

%%
%% -----------------------------------------------------------------------------
%% Serialization
%% -----------------------------------------------------------------------------
%%

serialize(Body, Req) ->
  % NB: we choose encoder from media_type meta, honoring Accept: header.
  % One may choose to always encode to one fixed format as well.
  {CType, _} = cowboy_req:meta(media_type, Req),
  encode(CType, Body).

%% NB: first argument should match those of content_types_*/2
encode({<<"application">>, <<"x-www-form-urlencoded">>, []}, Body) ->
  urlencode(Body);
encode({<<"application">>, <<"json">>, []}, Body) ->
%   jsx:encode(Body);
% encode(undefined, Body) ->
  jsx:encode(Body).

%% NB: Cowboy issue #479
urlencode(Bin) when is_binary(Bin) ->
  cowboy_http:urlencode(Bin);
urlencode(Atom) when is_atom(Atom) ->
  urlencode(atom_to_binary(Atom, latin1));
urlencode(Int) when is_integer(Int) ->
  urlencode(list_to_binary(integer_to_list(Int)));
urlencode({K, undefined}) ->
  << (urlencode(K))/binary, $= >>;
urlencode({K, V}) ->
  << (urlencode(K))/binary, $=, (urlencode(V))/binary >>;
urlencode(List) when is_list(List) ->
  binary_join([urlencode(X) || X <- List], << $& >>).

binary_join([], _Sep) ->
  <<>>;
binary_join([H], _Sep) ->
  << H/binary >>;
binary_join([H | T], Sep) ->
  << H/binary, Sep/binary, (binary_join(T, Sep))/binary >>.
