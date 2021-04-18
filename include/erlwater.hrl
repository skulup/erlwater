%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-author("Alpha Umaru Shaw").

-define(Undef, undefined).


-ifdef(USE_OLD_LOGGER).
%%-define(debug, true).
-ifdef(debug).
-define(LOG_DEBUG(Str, Args), ew_logger:log(info, Str, Args)).
-define(LOG_DEBUG(Str, Args), ew_logger:log(info, Str, Args)).
-define(LOG_INFO(Str, Args), ew_logger:log(info, Str, Args)).
-define(LOG_INFO(Str, Args), ew_logger:log(info, Str, Args)).
-else.
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_DEBUG(Str, Args), begin _ = Args end).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_INFO(Str, Args), begin _ = Args end).
-endif.
-define(LOG_NOTICE(Str, Args), ew_logger:log(info, Str, Args)).
-define(LOG_NOTICE(Str, Args), ew_logger:log(info, Str, Args)).
-define(LOG_WARNING(Str, Args), ew_logger:log(warning, Str, Args)).
-define(LOG_WARNING(Str, Args), ew_logger:log(warning, Str, Args)).
-define(LOG_ERROR(Str, Args), ew_logger:log(error, Str, Args)).
-define(LOG_ERROR(Str, Args), ew_logger:log(error, Str, Args)).
-else. % Use new logging API.
-include_lib("kernel/include/logger.hrl").
-endif.