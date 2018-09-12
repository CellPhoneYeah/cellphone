%% 在线玩家网关进程记录
-define(ETS_ONLINE_ROLE, ets_online_role).
-record(ets_online_role, {
          role_name,
          net_pid}).
