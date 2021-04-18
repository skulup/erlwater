%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(ew_app).
-author("Alpha Umaru Shaw").

-include("erlwater.hrl").

%% API
-export([get_bool_env/2, get_int_env/2, get_int_env/3, get_float_env/2, get_float_env/3, get_binary_env/2, get_binary_env/3]).

get_bool_env(App, Name) ->
  convert(
    fun(Val) ->
      erlwater:to_boolean(Val)
    end, App, Name, false, "boolean").

get_int_env(App, Name) ->
  ?MODULE:get_int_env(App, Name, ?Undef).

get_int_env(App, Name, Def) when is_integer(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_integer(Val)
    end, App, Name, ?Undef, "integer").

get_float_env(App, Name) ->
  ?MODULE:get_float_env(App, Name, ?Undef).

get_float_env(App, Name, Def) when is_float(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_float(Val)
    end, App, Name, ?Undef, "float").

get_binary_env(App, Name) ->
  ?MODULE:get_binary_env(App, Name, ?Undef).

get_binary_env(App, Name, Def) when is_binary(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_binary(Val)
    end, App, Name, ?Undef, "binary").

convert(ConvertFun, App, Key, Def, ExpectedType) ->
  Value = get_env(App, Key, Def),
  try
    ConvertFun(Value)
  catch
    _:_ ->
      throw_error(App, Key, ExpectedType, Value)
  end.

throw_error(App, Key, ExpectedType, Given) ->
  erlang:error("Env var: \"~s\" in ~p must be an ~p. Given: ~p", [Key, App, ExpectedType, Given]).

get_env(App, Param, Default) ->
  case application:get_env(App, Param) of
    undefined -> Default;
    {ok, undefined} -> Default;
    {ok, Value} -> Value
  end.