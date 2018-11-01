-module(role_sup).

-behaviour(supervisor).
-include("global.hrl").

%% API
-export([start_link/0]).

-export([
         start_role/2,
         stop_role/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_role(Role, NetPid) ->
    supervisor:start_child(?MODULE, [Role, NetPid]).

stop_role(RoleId) ->
    Pid = lib_role:pid(RoleId),
    supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?ETS_ROLE = ets:new(?ETS_ROLE, [set, public, named_table, {keypos, #tab_role.id}]), % 在线玩家列表
    ?ETS_NAME_ROLE_ID = ets:new(?ETS_NAME_ROLE_ID, [set, public, named_table, {keypos, #ets_name_role_id.role_name}]), % 名和id映射
    init_name_role_id(),
    RoleServer = ?CHILD(role_server, worker),
    {ok, { {simple_one_for_one, 5, 10}, [RoleServer]} }.

init_name_role_id() ->
    Key = lib_data:dirty_first(?TAB_ROLE),
    init_name_role_id(Key).

init_name_role_id(?EOT) ->
    ok;
init_name_role_id(Key) ->
    [Role] = lib_data:dirty_read(?TAB_ROLE, Key),
    #tab_role{
       id = RoleId,
       name = RoleName
      } = Role,
    role_server:add_name_role_id(RoleName, RoleId),
    init_name_role_id(lib_data:dirty_next(?TAB_ROLE, Key)).
