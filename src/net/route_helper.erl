-module(route_helper).

-include("global.hrl").

-export([
         route/1
        ]).

route(login_tos) -> login;
route(_) -> ?UNDEF.
