-record(state, {
    method :: binary(),
    params,
    body,
    query,
    auth,
    completed = false :: boolean(),
    options,
    handler :: module()
  }).
