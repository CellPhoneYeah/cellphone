-define(LOG_ERROR(Msg), error_logger:error_report(Msg)).
-define(LOG_ERROR(Msg, Arg), error_logger:error_report(Msg, Arg)).

-define(LOG_WARN(Msg), error_logger:warning_report(Msg)).
-define(LOG_WARN(Msg, Arg), error_logger:warning_report(Msg, Arg)).

-define(LOG_INFO(Msg), error_logger:info_report(Msg)).
-define(LOG_INFO(Msg, Arg), error_logger:info_report(Msg, Arg)).
