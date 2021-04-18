%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(ew_time).
-author("Alpha Umaru Shaw").

%% API
-export([millis/0, micros/0, nanos/0]).
-export([local_iso8601/0, universal_iso8601/0, iso8601/1]).

millis() -> os:system_time(milli_seconds).

micros() -> os:system_time(micro_seconds).

nanos() -> os:system_time(nano_seconds).

local_iso8601() ->
  ?MODULE:iso8601(calendar:local_time()).

universal_iso8601() ->
  ?MODULE:iso8601(calendar:universal_time()).

iso8601(DateTime) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
  L = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec]),
  unicode:characters_to_binary(L).
