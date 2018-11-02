-module(route_helper).

-include("global.hrl").

-export([
         route/1
        ]).

%% 根据不同的协议指定不同的处理模块,默认由模块的handle_tos/3处理

route(register_tos) -> login;
route(login_tos) -> login;
route(ping_tos) -> ping;
route(chat_tos) -> {role, mod_role_chat};
route(_) -> ?UNDEF.
