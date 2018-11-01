-module(lib_role).

-include("global.hrl").

-export([
         role_register/3,
         role_login/2,
         role_send_msg/2,
         register_name/1,
         pid/1
        ]).

-export([
        is_online/1,
        is_registered/1
        ]).

-export([
        get_all/0
        ]).

-export([
        send_proto/2,
        get_role/1
        ]).

pid(RoleId) ->
    RegisterName = register_name(RoleId),
    case is_online(RegisterName) of
        true ->
            RegisterName;
        false ->
            ?UNDEF
    end.

get_role(RoleId) ->
    lib_data:dirty_read(?TAB_ROLE, RoleId).

get_all() ->
    ets:match(?ETS_ROLE, {'$1', '_'}).

is_online(RoleId) when is_integer(RoleId) ->
    RegisterName = register_name(RoleId),
    is_online(RegisterName);
is_online(RegisterName) when is_atom(RegisterName) ->
    case whereis(RegisterName) of
        ?UNDEF ->
            false;
        _ ->
            true
    end.

register_name(RoleId) ->
    list_to_atom("role_" ++ integer_to_list(RoleId)).

role_register(RoleId, RoleName, Password) ->
    case role_server:get_role_id_by_name(RoleName) of
        [] ->
            lib_data:dirty_write(?TAB_ROLE, #tab_role{id = RoleId, name = RoleName, password = Password}),
            ok;
        _ ->
            has_used
    end.

role_login(Name, Password) ->
    case role_server:get_role_id_by_name(Name) of
        [] ->
            not_found;
        [RoleId] ->
            [#tab_role{password = PSW}] = lib_data:dirty_read(?TAB_ROLE, RoleId),
            if PSW =:= Password ->
                   NetPid = self(),
                   role_sup:start_child(RoleId, NetPid);
               true ->
                   password_wrong
            end
    end.

role_send_msg(Name, Msg) ->
    [RolePid ! {get_msg, Name, Msg} || RolePid <- role_server:all_roles()].

is_registered(RoleName) ->
    case role_server:get_role_id_by_name(RoleName) of
        ?UNDEF ->
            false;
        _ ->
            true
    end.

send_proto(RoleId, Proto) ->
    case is_online(RoleId) of
        true->
            RolePid = whereis(register_name(RoleId)),
            RolePid ! {binary, Proto};
        false ->
            ?UNDEF
    end.
