-module(lib_role).

-include("data.hrl").

-export([
         role_register/2,
         role_login/2,
         role_send_msg/2
        ]).

role_register(Name, Password) ->
    case lib_data:get_role_id_by_name(Name) of
        not_exists ->
            lib_data:insert_role(Name, Password),
            ok;
        _ ->
            has_used
    end.

role_login(Name, Password) ->
    case lib_data:get_role_id_by_name(Name) of
        not_exists ->
            not_found;
        RoleId ->
            #tab_role{password = PSW} = lib_data:get_role_by_id(RoleId),
            if PSW == Passwrod ->
                   NetPid = self(),
                   role_sup:start_child(RoleId, NetPid);
               true ->
                   password_wrong
            end
    end.

role_send_msg(Name, Msg) ->
    [RolePid ! {get_msg, Name, Msg} || RolePid <- role_server:all_roles()].
