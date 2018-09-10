-module(route_helper).

-export([
         get_routes/0
        ]).

get_routes() ->
    [
     {'_', [
            {"/", request_handler, []}
           ]}
    ].
