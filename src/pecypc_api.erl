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
    post_is_create/2,
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
  % apply apigen pragmatic REST recommendations
pecypc_log:info(cowboy_req:to_list(Req)),
  Req2 = cowboy_patch:patch_pragmatic_rest(
       cowboy_patch:patch_method(
       cowboy_patch:patch_headers(Req))),
  % extract request info
  {Params, Req3} = cowboy_req:bindings(Req2),
  {Query, Req4} = cowboy_req:qs_vals(Req3),
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
  % % {[<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"HEAD">>,
  % %     <<"OPTIONS">>, <<"PATCH">>, <<"POST">>], Req, State}.
  {_, Methods} = lists:keyfind(allow, 1, Opts),
  {Methods, Req, State}.

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
%% NB: Should be with respect to HTTP method.
%%
forbidden(Req, State = #state{handler = Handler}) ->
  case Handler:authorize(State) of
    {ok, State2} ->
      {false, Req, State2};
    error ->
      {true, Req, State}
  end.

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
%% Delegates actual processing to application's get/2.
%% Encodes response entity.
%%
%%
%% - {Body, Req, State} --> 200 OK
%% - {halt, Req, State} --> no further processing
%%
get_resource(Req, State = #state{handler = Handler}) ->
  case Handler:get(State) of
    {ok, Result} ->
      {serialize(Result, Req), Req, State};
    {goto, Location} ->
      {halt, cowboy_req:set_resp_header(<<"location">>, Location, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State}
  end.

%%
%% Route POST to put_*.
%%
post_is_create(Req, State) ->
  {true, Req, State}.

%%
%% NB: these handlers is also called on PATCH and POST.
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
%% Delegates actual processing to application's put/2.
%% Encodes possible response entity.
%%
put_resource(Req, State = #state{handler = Handler}) ->
% pecypc_log:info(cowboy_req:to_list(Req)),
  % @todo honor pragmatic rest switches to include body in the response etc.
  case Handler:put(State) of
    ok ->
      {true, Req, State};
    {ok, Body} ->
      {true, set_resp_body(Body, Req), State};
    % @todo process_put/2 should not know about HTTP terms like "location" etc.
    {goto, Location} ->
      {true, cowboy_req:set_resp_header(<<"location">>, Location, Req), State};
    error ->
      {halt, respond(400, undefined, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State}
  end.

%%
%% Delegates actual processing to application's delete/2.
%% Encodes possible response entity.
%%
%%
%% NB: must be defined if DELETE in allowed methods or 500 Internal Server Error
%% It should start deleting the resource and return.
%% - {true, Req, State} --> 204 No Content, unless delete_completed/2 defined
%% - {X =/= true, Req, State} --> 500 Internal Server Error
%% - {halt, Req, State} --> no further processing
%%
delete_resource(Req, State = #state{handler = Handler}) ->
  case Handler:delete(State) of
    ok ->
      {true, Req, State#state{completed = true}};
    accepted ->
      {true, Req, State#state{completed = false}};
    error ->
      {halt, respond(400, undefined, Req), State};
    {error, Reason} ->
      {halt, respond(400, Reason, Req), State}
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
%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------
%%

respond(Status, Reason, Req) ->
  {ok, Req2} = cowboy_req:reply(Status,
        set_resp_body(reason(Reason), Req)),
  Req2.

set_resp_body(Body, Req) ->
  cowboy_req:set_resp_body(serialize(reason(Body), Req), Req).

reason(undefined) ->
  reason(<<"unknown">>);
reason(Reason) when is_list(Reason) ->
  Reason;
reason(Reason) when is_binary(Reason); is_number(Reason) ->
  reason([{error, Reason}]);
reason(Reason) when is_atom(Reason) ->
  reason(atom_to_binary(Reason, latin1)).

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
urlencode(List) when is_list(List) ->
  urlencode(List, <<>>).
urlencode([], Acc) ->
  Acc;
urlencode([{K, V} | T], <<>>) ->
  urlencode(T, << (urlencode(K))/binary, $=, (urlencode(V))/binary >>);
urlencode([{K, V} | T], Acc) ->
  urlencode(T, << Acc/binary, $&,
    (urlencode(K))/binary, $=, (urlencode(V))/binary >>).
