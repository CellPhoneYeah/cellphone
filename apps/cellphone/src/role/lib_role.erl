-module(lib_role).

-include("global.hrl").

-export([
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
        get_role_in_db/1
        ]).

%%% ======
%%% API
%%% ======
%% 获取用户进程pid(注册名)
pid(RoleId) ->
    RegisterName = register_name(RoleId),
    case whereis(RegisterName) of
        _Pid when is_pid(_Pid) ->
            RegisterName;
        _ ->
            ?UNDEF
    end.

%% 从数据库获取用户数据
get_role_in_db(RoleId) ->
    lib_data:dirty_read(?TAB_ROLE, RoleId).

%% 获取所有在线用户id
get_all() ->
    ets:match(?ETS_ROLE, {'$1', '_'}).

%% 判断用户进程是否存在
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

%% 获取用户进程注册名
register_name(RoleId) ->
    list_to_atom("role_" ++ integer_to_list(RoleId)).

%% 检查用户名是否已经被占用
is_registered(RoleName) ->
    case role_server:get_role_id_by_name(RoleName) of
        ?UNDEF ->
            false;
        _ ->
            true
    end.

%% 发送协议到用户进程
send_proto(RoleId, Proto) ->
    case pid(RoleId) of
        RolePid when is_pid(RolePid) ->
            RolePid ! {binary, Proto};
        ?UNDEF ->
            ok
    end.
