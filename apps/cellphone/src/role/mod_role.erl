-module(mod_role).

-include("global.hrl").

-export([
            add_new_role/3,
            handle_tos/3
        ]).

-export([
        get_role/0,
        set_role/1
        ]).

%%% ======
%%% API
%%% ======
add_new_role(RoleId, RoleName, Psd) ->
    NewRole = #tab_role{
                 id = RoleId,
                 name = RoleName,
                 password = Psd},
    lib_data:dirty_write(?TAB_ROLE, NewRole).

set_role(Role) ->
    erlang:put({?MODULE, role}, Role).

get_role() ->
    erlang:get({?MODULE, role}).



%%% ======
%%% call back
%%% ======
handle_tos(_RoleId, _NetPid, _Proto) ->
    ?LOG_INFO("success"),
    ok.
