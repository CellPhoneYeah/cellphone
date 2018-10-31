%%%-------------------------------------------------------------------
%% @doc cp_robot public API
%% @end
%%%-------------------------------------------------------------------

-module(cp_robot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start() ->
    case application:start(cp_robot) of
        ok ->
            cp_robot_sup:start_robot(1);
        Error ->
            io:format("start fail ~p~n", [Error])
    end.

start(_StartType, _StartArgs) ->
    cp_robot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
