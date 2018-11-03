%%%-------------------------------------------------------------------
%% @doc cp_robot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cp_robot_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_robot/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_robot(1) ->
    supervisor:start_child(?SERVER, [1]);
start_robot(Num) ->
    supervisor:start_child(?SERVER, [Num]),
    start_robot(Num -1).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Robot = ?CHILD(robot_client, worker),
    {ok, { {simple_one_for_one, 0, 1}, [Robot]} }.

%%====================================================================
%% Internal functions
%%====================================================================
