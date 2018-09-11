-include("data.hrl").

-define(IF(A, B, C), case A of true -> B; false -> C end).
