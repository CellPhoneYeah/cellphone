-module(role_sup).

-behaviour(supervisor).
-include("global.hrl").

%% API
-export([start_link/0]).

-export([start_role/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_role(RoleId, NetPid) ->
    supervisor:start_child(?MODULE, [RoleId, NetPid]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?ETS_ROLE = ets:new(?ETS_ROLE, [set, protected, named_table, {keypos, #tab_role.id}]), % 在线玩家列表
    ?ETS_NAME_ROLE_ID = ets:new(?ETS_NAME_ROLE_ID, [set, protected, named_table, {keypos, #ets_name_role_id.role_name}]), % 名和id映射
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
    ets:insert(?ETS_NAME_ROLE_ID, #ets_name_role_id{role_name = RoleName, role_id = RoleId}),
    init_name_role_id(lib_data:next(?TAB_ROLE, Key)).
