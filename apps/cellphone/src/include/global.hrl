-include("data.hrl").
-include("phone.hrl").
-include("ets.hrl").
-include("ecode.hrl").
-include("log.hrl").

-define(UNDEF, undefined).

-define(EOT, '$end_of_table').

-define(IF(A, B, C), case A of true -> B; false -> C end).

-define(THROW(Error), throw({error, Error})).
-define(THROW(Error, Args), throw({error, io_lib:format(Error,Args)})).

-define(MIN_SECOND, 60).
-define(HOUR_SECOND, 3600).
-define(DAY_SECOND, 86400).
