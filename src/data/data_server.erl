-module(data_server).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/0
        ]).

%%% =====
%%% API
%%% =====
start_link() ->
    io:format("start data server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% =====
%%% call back
%%% =====
init([]) ->
    erlang:process_flag(trap_exit, true),
    do_init(),
    wait_for_tables(),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_State, _Reason) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
do_init() ->
    
