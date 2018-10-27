-module(net_login).

-include("global.hrl").

-export([
            set_last_login_time/1,
            get_last_login_time/0,
            set_role_pid/1,
            get_role_pid/0,
            get_role_id/0,
            set_role_id/1,
            login/2
        ]).

-define(LAST_LOGIN_TIME, last_login_time).
-define(ROLE_PID, role_pid).
-define(ROLE_ID, role_id).

set_last_login_time(Time) ->
    erlang:put(?LAST_LOGIN_TIME, Time).

get_last_login_time() ->
    erlang:get(?LAST_LOGIN_TIME).

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
            {ok, #register_toc{code = ?E_OK, role = #s_role{role_id = RoleId, role_name = RoleName}}};
        {error, Code} ->
            {fail, #register_toc{code = Code}}
    end;
login(#login_tos{role_name = RoleName, psd = Psd}, NetPid) ->
    RoleId = lib_role:get_id_by_name(RoleName),
    if
        RoleId =/= ?UNDEF ->
    case lib_role:is_online(RoleId) of
        true ->
            net_misc:stop_net_pid(RoleId);
        false ->
            ok
    end,
    case check_login(RoleId, Psd) of
        {ok, #tab_role{name = RoleName} = Role}->
            role_sup:start_role(Role, NetPid),
            {ok, #login_toc{code = ?E_OK, role = #s_role{role_id = RoleId, role_name = RoleName}}};
        {error, Code} ->
            {fail, #login_toc{code = Code}}
    end;
        true ->
            {fail, #login_toc{code = ?E_LOGIN_ROLE_NAME_NOT_REGISTERED}}
    end.

check_login(RoleId, Psd) ->
    case net_server:get_role(RoleId) of
        [#tab_role{id = RoleId, password = Psd} = Role] ->
            {ok, Role};
        [#tab_role{id = RoleId}] ->
            {error, ?E_LOGIN_PASSWORD_NOT_MATCH};
        _ ->
            {error, ?E_LOGIN_ROLE_NOT_FOUND}
    end.

check_register(RoleName, _Psd) ->
    case ets:lookup(?ETS_NAME_ROLE_ID, RoleName) of
        [] ->
            RoleId = net_id_manager:get_id(),
            {ok, RoleId};
        _ ->
            {fail, ?E_REGISTER_NAME_HAS_BEEN_USED}
    end.
