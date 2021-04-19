%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2021 1:58 AM
%%%-------------------------------------------------------------------
-module(ew_string).
-author("kaja").

%% API
-export([trim/1]).


trim(Uri) ->
  case catch string:trim(Uri) of
    Uri1 when is_list(Uri1) ->
      Uri1;
    _ ->
      %% `string:trim/1` undefined
      string:strip(Uri)
  end.