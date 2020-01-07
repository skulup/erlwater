%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(erlwater_env).
-author("Alpha Umaru Shaw").

%% API
-export([get_int_env/3, get_float_env/3, get_env/3]).


get_float_env(App, Param, Default) when is_integer(Default) ->
  case get_env(App, Param, Default) of
    Val when is_float(Val) -> Val;
    Other ->
      try_convert(
        fun() ->
          erlwater:to_float(Other)
        end, App, Param, Other, "float")
  end.

get_int_env(App, Param, Default) when is_integer(Default) ->
  case get_env(App, Param, Default) of
    Val when is_integer(Val) -> Val;
    Other ->
      try_convert(
        fun() ->
          erlwater:to_integer(Other)
        end, App, Param, Other, "integer")
  end.


get_env(App, Param, Default) ->
  case application:get_env(App, Param) of
    undefined -> Default;
    {ok, undefined} -> Default;
    {ok, Value} -> Value
  end.

try_convert(ConvertFun, App, Param, Given, Type) ->
  try
    ConvertFun()
  catch
    _:_ ->
      throw_error(App, Param, Given, Type)
  end.

throw_error(App, Param, Given, Type) ->
  error("Param: ~p in ~p must be an ~p. Given: ~p", [Param, App, Type, Given]).
