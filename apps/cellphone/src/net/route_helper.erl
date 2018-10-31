-module(route_helper).

-include("global.hrl").

-export([
         route/1
        ]).

route(register_tos) -> login;
route(login_tos) -> login;
route(ping_tos) -> login;
route(_) -> ?UNDEF.
