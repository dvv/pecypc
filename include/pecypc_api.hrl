-record(state, {
    method :: binary(),
    params,
    body,
    query,
    auth,
    completed :: boolean,
    options,
    handler :: module()
  }).
