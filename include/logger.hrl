%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 12:16 PM
%%%-------------------------------------------------------------------
-author("kaja").

-ifndef(USE_ERROR_LOGGER). %% New logging API.
-include_lib("kernel/include/logger.hrl").
-else. %% Use error_logger
-ifndef(debug).
-define(LOG_DEBUG(Text, Args), ok).
-define(LOG_DEBUG(Text, Args), ok).
-define(LOG_INFO(Text, Args), ok).
-define(LOG_INFO(Text, Args), ok).
-else.
-define(LOG_DEBUG(Text, Args), ew_logger:log(info, Text, Args)).
-define(LOG_DEBUG(Text, Args), ew_logger:log(info, Text, Args)).
-define(LOG_INFO(Text, Args), ew_logger:log(info, Text, Args)).
-define(LOG_INFO(Text, Args), ew_logger:log(info, Text, Args)).
-endif.
-define(LOG_NOTICE(Text, Args), ew_logger:log(info, Text, Args)).
-define(LOG_NOTICE(Text, Args), ew_logger:log(info, Text, Args)).
-define(LOG_WARNING(Text, Args), ew_logger:log(warning, Text, Args)).
-define(LOG_WARNING(Text, Args), ew_logger:log(warning, Text, Args)).
-define(LOG_ERROR(Text, Args), ew_logger:log(error, Text, Args)).
-define(LOG_ERROR(Text, Args), ew_logger:log(error, Text, Args)).
-endif.