-module(role_sup).

-behaviour(supervisor).
-include("role.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_role(RoleId, Pid) ->
    supervisor:start_child(?MODULE, [RoleId, Pid]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?ETS_ONLINE_ROLE = ets:new(?ETS_ONLINE_ROLE, [set, protected, named_table, {keypos, #ets_online_role.role_name}]),
    RoleServer = ?CHILD(role_server, worker),
    {ok, { {simple_one_for_one, 5, 10}, [RoleServer]} }.

