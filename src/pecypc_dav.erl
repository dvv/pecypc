-module(pecypc_dav).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

% -behaviour(cowboy_rest_handler).
-export([
    init/3,
    rest_init/2,
    malformed_request/2,
    allowed_methods/2,
    is_authorized/2,
    resource_exists/2,
    content_types_accepted/2,
    content_types_provided/2,
    % charsets_provided/2,
    delete_resource/2
  ]).

-export([
    get_text/2,
    put_text/2
  ]).

-include_lib("kernel/include/file.hrl").

-record(state, {
    auth,
    exists,
    has_slash,
    path,
    type
  }).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {_, Auth} = lists:keyfind(auth, 1, Opts),
  {_, Root} = lists:keyfind(root, 1, Opts),
  {PathInfo, Req2} = cowboy_req:path_info(Req),
  [Path] = cowboy_req:get([path], Req2),
  HasSlash = binary:last(Path) =:= $/,
  Directory = fullpath(filename:absname(Root)),
  Filepath = filename:join([Directory | PathInfo]),
  Len = byte_size(Directory),
pecypc_log:info({path, Directory, Filepath}),
  case fullpath(Filepath) of
    << Directory:Len/binary, $/, _/binary >> ->
      {ok, Req2, #state{
        auth = Auth,
        has_slash = HasSlash,
        path = Filepath
      }};
    Directory ->
      {ok, Req2, #state{
        auth = Auth,
        has_slash = HasSlash,
        path = Filepath
      }};
    _ ->
      {ok, Req2, #state{
        auth = Auth,
        has_slash = HasSlash,
        path = error
      }}
  end.

malformed_request(Req, State = #state{path = error}) ->
  {true, Req, State};
malformed_request(Req, State) ->
  {false, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State = #state{auth = Auth}) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, Auth}, Req2} ->
      {true, Req2, State};
    _ ->
      {{false, <<"Basic">>}, Req, State}
  end.

content_types_provided(Req, State) ->
  {[
    {{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]}, get_text}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]}, put_text}
  ], Req, State}.

resource_exists(Req, State = #state{path = Path}) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = Type}} ->
      {true, Req, State#state{exists = true, type = Type}};
    {error, _Reason} ->
      {false, Req, State#state{exists = false}}
  end.

delete_resource(Req, State = #state{has_slash = true, path = Path,
    type = directory}) ->
pecypc_log:info({deldir, Path}),
  case file:del_dir(Path) of
    ok ->
      {true, Req, State};
    {error, Reason} ->
      {halt, fail(Reason, Req), State}
  end;
delete_resource(Req, State = #state{has_slash = false, path = Path,
    type = regular}) ->
pecypc_log:info({delete, Path}),
  case file:delete(Path) of
    ok ->
      {true, Req, State};
    {error, Reason} ->
      {halt, fail(Reason, Req), State}
  end;
delete_resource(Req, State) ->
  {halt, forbid(<<>>, Req), State}.

%% -----------------------------------------------------------------------------
%% Application handlers.
%% -----------------------------------------------------------------------------

get_text(Req, State = #state{has_slash = true, path = Path,
    type = directory}) ->
  case file:list_dir(Path) of
    {ok, List} ->
      % {binary_join_nl(List), Req, State};
      {[[E, $\n] || E <- List], Req, State};
    {error, Reason} ->
      {halt, fail(Reason, Req), State}
  end;
get_text(Req, State = #state{has_slash = true, path = Path,
    type = regular}) ->
pecypc_log:info({get, Path}),
  case file:read_file(Path) of
    {ok, Body} ->
      {Body, Req, State};
    {error, Reason} ->
      {halt, fail(Reason, Req), State}
  end;
get_text(Req, State) ->
  {halt, forbid(<<>>, Req), State}.

put_text(Req, State = #state{exists = false, has_slash = true}) ->
  {PathInfo, Req2} = cowboy_req:path_info(Req),
  case mkdirp(PathInfo) of
    ok ->
      {true, Req2, State};
    {error, Reason} ->
      {halt, fail(Reason, Req2), State}
  end;
put_text(Req, State = #state{exists = false, has_slash = false}) ->
  {PathInfo, Req2} = cowboy_req:path_info(Req),
  case mkdirp(lists:sublist(PathInfo, length(PathInfo) - 1)) of
    ok ->
      put_text(Req2, State#state{exists = true});
    {error, Reason} ->
      {halt, fail(Reason, Req2), State}
  end;
put_text(Req, State = #state{exists = true, path = Path, type = regular}) ->
pecypc_log:info({put, Path}),
  {ok, Body, Req2} = cowboy_req:body(Req),
  case file:write_file(Path, Body) of
    ok ->
      {true, Req2, State};
    {error, Reason} ->
      {halt, fail(Reason, Req2), State}
  end.

%% -----------------------------------------------------------------------------
%% Helpers.
%% -----------------------------------------------------------------------------

fullpath(Path) when is_binary(Path) ->
  fullpath(filename:split(Path), []).
fullpath([], Acc) ->
  filename:join(lists:reverse(Acc));
fullpath([<<".">>|Tail], Acc) ->
  fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], Acc=[_]) ->
  fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], [_|Acc]) ->
  fullpath(Tail, Acc);
fullpath([Segment|Tail], Acc) ->
  fullpath(Tail, [Segment|Acc]).

mkdirp(Bins) when is_list(Bins) ->
  file:make_dir([binary_to_list(B) || B <- Bins]).

% binary_join_nl([]) ->
%   <<>>;
% binary_join_nl(B) when is_binary(B) ->
%   B;
% binary_join_nl(L) when is_list(L) ->
%   << $\n, R/binary >> = << << $\n, (binary_join_nl(E))/binary >> || E <- L >>,
%   R.

fail(Code, Reason, Req) when is_atom(Reason) ->
  fail(Code, atom_to_binary(Reason, latin1), Req);
fail(Code, Reason, Req) ->
  {ok, Req2} = cowboy_req:reply(Code, [], Reason, Req),
  Req2.

fail(Reason, Req) ->
  fail(400, Reason, Req).

forbid(Reason, Req) ->
  fail(403, Reason, Req).
