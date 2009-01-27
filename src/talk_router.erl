%%%%%
% Routes ips to pid files
%%%%%

-module (talk_router).
-author ("Ari Lerner").

-behaviour(gen_server).

-import(ets).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).