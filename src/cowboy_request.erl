%%
%%------------------------------------------------------------------------------
%% !!! Hackish reuse of cowboy_client undocumented interface !!!
%%------------------------------------------------------------------------------
%%

-module(cowboy_request).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([request/4]).

-record(client, {
  state = wait :: wait | request | response | response_body,
  opts = [] :: [any()],
  socket = undefined :: undefined | inet:socket(),
  transport = undefined :: module(),
  timeout = 5000 :: timeout(), %% @todo Configurable.
  buffer = <<>> :: binary(),
  connection = keepalive :: keepalive | close,
  version = {1, 1} :: cowboy_http:version(),
  response_body = undefined :: undefined | non_neg_integer()
}).

request(Method, URL, Headers, Body) ->
pecypc_log:info({req, Method, URL, Body}),
  {ok, Client} = cowboy_client:init([]),
  {ok, Client2} = cowboy_client:request(Method, URL, [
    ], Body, Client),
  Result = case cowboy_client:response(Client2) of
    {ok, Status, ResHeaders, Client3} ->
pecypc_log:info({resp, Status, ResHeaders}),
      case Client3#client.state of
        % @fixme dirty hack, reports only first read chunk
        request ->
pecypc_log:info({buffer, Client3}),
          {ok, Status, Client3#client.buffer};
        response_body ->
          case cowboy_client:response_body(Client3) of
            {ok, ResBody, _} ->
pecypc_log:info({body, ResBody}),
              % @todo analyze Status
              {ok, Status, ResBody};
            Else ->
              Else
          end
      end;
    _Else ->
pecypc_log:info({reqerr, _Else}),
      {error, <<"server_error">>}
  end,
pecypc_log:info({res, Result}),
  Result.
