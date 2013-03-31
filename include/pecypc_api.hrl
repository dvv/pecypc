-type proplist() :: list({term(), term()}).

-record(state, {
    method :: binary(),
    params :: proplist(),
    body :: proplist(),
    query :: proplist(),
    auth :: {Identity :: term(), AllowedScope :: term()},
    completed = false :: boolean(),
    options :: proplist(),
    handler :: module()
  }).
