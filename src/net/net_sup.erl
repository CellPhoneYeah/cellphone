-module(net_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CowBoy = ?CHILD(net_server, worker),
    IdServer = ?CHILD(id_server, worker),
    {ok, { {one_for_one, 5, 10}, [CowBoy, IdServer]}}.
