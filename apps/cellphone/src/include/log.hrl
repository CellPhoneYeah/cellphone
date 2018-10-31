-define(ADD_HEAD(Msg), io_lib:format("module:~p line:~p " ++ Msg, [?MODULE, ?LINE])).
-define(ADD_HEAD(Msg, Args), io_lib:format("module:~p line:~p " ++ Msg, [?MODULE, ?LINE] ++ Args)).

-define(LOG(Lv, Pid, Msg), lager:log(Lv, Pid, ?ADD_HEAD(Msg))).
-define(LOG(Lv, Pid, Msg, Args), lager:log(Lv, Pid, ?ADD_HEAD(Msg, Args))).

-define(LOG_ERROR(Msg), ?LOG(error, self(), Msg)).
-define(LOG_ERROR(Msg, Arg), ?LOG(error, self(), Msg, Arg)).

-define(LOG_WARN(Msg), ?LOG(warning, self(), Msg)).
-define(LOG_WARN(Msg, Arg), ?LOG(warning, self(), Msg, Arg)).

-define(LOG_INFO(Msg), ?LOG(info, self(), Msg)).
-define(LOG_INFO(Msg, Arg), ?LOG(info, self(), Msg, Arg)).


