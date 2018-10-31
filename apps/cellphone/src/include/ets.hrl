%% 在线玩家信息
-define(ETS_ROLE, ets_role).

%% 注册用户的名和id的映射
-define(ETS_NAME_ROLE_ID, ets_name_role_id).
-record(ets_name_role_id, {
         role_name,
         role_id}).

-define(ETS_ID_MANAGE, ets_id_manage).
-record(?ETS_ID_MANAGE, {
          key,
          value}).

%% 在线玩家网关记录
-define(ETS_NETPID, ets_netpid).
-record(?ETS_NETPID, {
           role_id,
           netpid}).
