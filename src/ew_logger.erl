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
-export([log/3]).

-spec log(info | warning | error, iodata() | atom() | map(), list()) -> ok.

log(Level, Text, Args) ->
  ConfiguredLevel =
  case application:get_env(erlwater, logLevel, 'info') of
    {ok, L} -> L;
    _ -> info
  end,
  LevelInt = logLevelInt(Level),
  ConfiguredLevelInt = logLevelInt(ConfiguredLevel),
  if ConfiguredLevelInt >= LevelInt ->
    do_log(Level, Text, Args);
    true ->
      ok
  end.

do_log(Level, Text, Args) ->
  case Level of
    info ->
      error_logger:info_msg(Text, Args);
    warning ->
      error_logger:warning_msg(Text, Args);
    error ->
      error_logger:error_msg(Text, Args)
  end.

logLevelInt(Level) ->
  case Level of
    error -> 1;
    warning -> 2;
    info -> 3
  end.
