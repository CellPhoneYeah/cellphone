-define(PRINT(Msg, Args), io:format("module:~p line:~p " ++ Msg ++ "~n", [?MODULE, ?LINE] ++ Args)).
-define(PRINT(Msg), io:format("module:~p line:~p " ++ Msg ++ "~n", [?MODULE, ?LINE])).

-record(state, {
          buffer = <<>>,
          num,  % 编号
          id,   % id
          name, % 名字
          psd = "123", % 密码
          socket    % socket
         }).
