-module(net_login).

-include("global.hrl").

-export([
            set_last_ping_time/1,
            get_last_ping_time/0,
            set_role_pid/1,
            get_role_pid/0,
            get_role_id/0,
            set_role_id/1,
            login/2
        ]).

-define(LAST_PING_TIME, last_ping_time).
-define(ROLE_PID, role_pid).
-define(ROLE_ID, role_id).

set_last_ping_time(Time) ->
    erlang:put(?LAST_PING_TIME, Time).

get_last_ping_time() ->
    erlang:get(?LAST_PING_TIME).

set_role_pid(RoleId) ->
    erlang:put(?ROLE_PID, RoleId).

get_role_pid() ->
    erlang:get(?ROLE_PID).

get_role_id() ->
    erlang:get(?ROLE_ID).

set_role_id(RoleId) ->
    erlang:put(?ROLE_ID, RoleId).

login(#register_tos{role_name = RoleName, psd = Psd}, _NetPid) ->
    case check_register(RoleName, Psd) of
        {ok, RoleId}->
            do_register(RoleId, RoleName, Psd),
            {ok, #register_toc{code = ?E_OK, role = #s_role{role_id = RoleId, role_name = RoleName}}};
        {error, Code} ->
            {fail, #register_toc{code = Code}}
    end;
login(#login_tos{role_name = RoleName, psd = Psd}, NetPid) ->
    RoleId = role_server:get_role_id_by_name(RoleName),
    case catch check_login(RoleId, Psd) of
        {ok, #tab_role{name = RoleName} = Role}->
            do_login(Role, NetPid),
            {ok, #login_toc{code = ?E_OK, role = #s_role{role_id = RoleId, role_name = RoleName}}};
        {error, Code} ->
            {ok, #login_toc{code = Code}};
        Error ->
            ?LOG_ERROR("~p", [Error]),
            {ok, #login_toc{code = ?E_SYSTEM_ERROR}}
    end.

do_login(#tab_role{id = RoleId} = Role, NetPid) ->
    {ok, _} = role_sup:start_role(Role, NetPid),
    net_server:add_role_netpid(RoleId, NetPid).

check_login(RoleId, Psd) ->
    if
        RoleId =/= ?UNDEF ->
            case lib_role:is_online(RoleId) of
                true ->
                    net_server:stop(RoleId), % 停掉网关
                    role_sup:stop_role(RoleId); % 停掉用户进程
                false ->
                    ok
            end;
        true ->
            ?THROW(?E_LOGIN_ROLE_NAME_NOT_REGISTERED)
    end,
    case lib_role:get_role(RoleId) of
        [#tab_role{id = RoleId, password = Psd} = Role] ->
            {ok, Role};
        [#tab_role{id = RoleId}] ->
            ?THROW(?E_LOGIN_PASSWORD_NOT_MATCH);
        _ ->
            ?THROW(?E_LOGIN_ROLE_NOT_FOUND)
    end.

check_register(RoleName, _Psd) ->
    case lib_role:is_registered(RoleName) of
        false ->
            RoleId = id_server:get_max_id(?ETS_ROLE),
            {ok, RoleId};
        true ->
            ?THROW(?E_REGISTER_NAME_HAS_BEEN_USED)
    end.

do_register(RoleId, RoleName, Psd) ->
    mod_role:add_new_role(RoleId, RoleName, Psd),
    role_server:add_name_role_id(RoleName, RoleId).
