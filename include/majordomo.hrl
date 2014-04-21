%%%-------------------------------------------------------------------
%%% @author erikh
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. apr 2014 18:13
%%%-------------------------------------------------------------------
-author("erikh").

-define(MDP_CLIENT_HEADER, <<"MDPC01">>).
-define(MDP_WORKER_HEADER, <<"MDPW01">>).

-define(MDP_READY_CMD, <<1>>).
-define(MDP_REQUEST_CMD, <<2>>).
-define(MDP_REPLY_CMD, <<3>>).
-define(MDP_HEARTBEAT_CMD, <<4>>).
-define(MDP_DISCONNECT_CMD, <<5>>).

-define(HEARTBEAT_INTERVAL, 2500).
-define(HEARTBEAT_LIVENESS, 3).

-define(MMI_SERVICE, <<"mmi.service">>).

-define(MMI_FOUND, <<"200">>).
-define(MMI_NOT_FOUND, <<"404">>).
-define(MMI_NOT_IMPLEMENTED, <<"501">>).
