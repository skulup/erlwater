%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2021 8:14 AM
%%%-------------------------------------------------------------------
-module(ew_logger).
-author("kaja").

%% API
-ifdef(USE_ERROR_LOGGER).
-export([log/3]).

-spec log(info | warning | error, iodata() | atom() | map(), list()) -> ok.
log(Level, Text, Args) ->
  case Level of
    info ->
      error_logger:info_msg(Text, Args);
    warning ->
      error_logger:warning_msg(Text, Args);
    error ->
      error_logger:error_msg(Text, Args)
  end.
-endif.
