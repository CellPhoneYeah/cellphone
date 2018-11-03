-module(chat_server).

-include("global.hrl").

-behaviour(gen_server).

-export([
         init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         add_chat_records/1,
         set_chat_records/1,
         get_chat_records/0
        ]).

-define(CHAT_RECORDS, chat_record).
-define(CHAT_RECORDS_MAX_NUM, 10). % 最大聊天记录数量

%%% ========
%%% API
%%% ========
add_chat_records(Record) ->
    gen_server:cast(?MODULE, {add_chat_records, Record}).

set_chat_records(Records) ->
    gen_server:cast(?MODULE, {set_chat_records, Records}).

get_chat_records() ->
    gen_server:call(?MODULE, get_chat_records).

%%% ====
%%% call back
%%% ====
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    set_chat_records([]),
    {ok, []}.

handle_call(Request, _From, State) ->
    case catch do_handle_call(Request) of
        {ok, Respond} ->
            {reply, Respond, State};
        stop ->
            {stop, normal, State};
        Error ->
            ?LOG_ERROR("~p", [Error]),
            {noreply, State}
    end.

handle_cast(Request, State) ->
    case catch do_handle_cast(Request) of
        ok ->
            {noreply, State};
        stop ->
            {stop, normal, State};
        Error ->
            ?LOG_ERROR("~p", [Error]),
            {noreply, State}
    end.

handle_info(Request, State) ->
    case catch do_handle_info(Request) of
        ok ->
            {noreply, State};
        stop ->
            {stop, normal, State};
        Error ->
            ?LOG_ERROR("~p", [Error]),
            {noreply, State}
    end.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ========
%%% internal
%%% ========
do_handle_call(get_chat_records) ->
    {ok, erlang:get({?MODULE, ?CHAT_RECORDS})};
do_handle_call(_Request) ->
    ?LOG_INFO("unknown request ~p", [_Request]),
    {ok, ok}.

do_handle_cast({add_chat_records, Record}) ->
    Records = get_chat_records(),
    Len = length(Records),
    NewRecords = 
    if
        ?CHAT_RECORDS_MAX_NUM > Len ->
            RevRecords = lists:reverse(Records),
            [_Last | Left] = RevRecords,
            [Record | lists:reverse(Left)];
        true ->
            [Record | Records]
    end,
    set_chat_records(NewRecords),
    ok;
do_handle_cast({set_chat_records, Records}) ->
    erlang:put({?MODULE, ?CHAT_RECORDS}, Records);
do_handle_cast(_Request) ->
    ?LOG_INFO("unknown request ~p", [_Request]),
    ok.

do_handle_info(_Request) ->
    ?LOG_INFO("unknown request ~p", [_Request]),
    ok.
