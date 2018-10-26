-module(lib_role).

-include("global.hrl").

-export([
         role_register/2,
         role_login/2,
         role_send_msg/2,
         register_name/1
        ]).

-export([
        is_online/1,
        is_registered/1
        ]).

-export([
        add_online_role/1,
        del_online_role/1
        ]).

add_online_role(Role) ->
    ets:insert(?ETS_ROLE, Role).

del_online_role(RoleId) ->
    ets:delete(?ETS_ROLE, RoleId).

is_online(RoleId) ->
    RegisterName = register_name(RoleId),
    case whereis(RegisterName) of
        ?UNDEF ->
            false;
        _ ->
            true
    end.

register_name(RoleId) ->
    list_to_atom("role_" ++ integer_to_list(RoleId)).

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
            if PSW == Password ->
                   NetPid = self(),
                   role_sup:start_child(RoleId, NetPid);
               true ->
                   password_wrong
            end
    end.

role_send_msg(Name, Msg) ->
    [RolePid ! {get_msg, Name, Msg} || RolePid <- role_server:all_roles()].

is_registered(RoleName) ->
    case ets:lookup(?ETS_NAME_ROLE_ID, RoleName) of
        [] ->
            false;
        _ ->
            true
    end.
