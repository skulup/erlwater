%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(erlwater_time).
-author("Alpha Umaru Shaw").

%% API
-export([local_iso8601/0, universal_iso8601/0, milliseconds/0, microseconds/0]).


local_iso8601() ->
  iso8601(calendar:local_time()).


universal_iso8601() ->
  iso8601(calendar:universal_time()).


iso8601(DateTime) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
  L = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    [Year, Month, Day, Hour, Min, Sec]),
  unicode:characters_to_binary(L).


milliseconds() ->
  erlang:system_time(milli_seconds).


microseconds() ->
  erlang:system_time(micro_seconds).