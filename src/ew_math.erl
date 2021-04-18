%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2021 7:07 AM
%%%-------------------------------------------------------------------
-module(ew_math).
-author("kaja").

%% API
-export([round/2]).

round(Number, Precision) when is_number(Number), is_number(Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.

