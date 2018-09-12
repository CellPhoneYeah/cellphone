-module(lib_role).

-include("data.hrl").

-export([
         role_register/2,
         role_login/2,
         role_send_msg/2
        ]).

role_register(Name, Password) ->
    case lib_data:get_role_id_by_name(Name) of
        not_found ->
            NewRole = #tab_role{name = Name, password = Password}, % id由内部生成
            lib_data:insert_role(NewRole),
            ok;
        _ ->
            has_used
    end.

role_login(Name, Password) ->
    case lib_data:get_role_id_by_name(Name) of
        not_found ->
            not_found;
        RoleId ->
            Role = lib_data:get_role_by_id(RoleId),
            NetPid = self(),
            role_server:role_login(Role, NetPid),
            ok
    end.

role_send_msg(Name, Msg) ->
    [RolePid ! {get_msg, Name, Msg} || RolePid <- role_server:all_roles()].
