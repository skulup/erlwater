%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2021 7:14 AM
%%%-------------------------------------------------------------------
-module(ew_os).
-author("kaja").

-include("erlwater.hrl").

%% API
-export([get_env/1, get_env/2, get_bool_env/1,
         get_int_env/1, get_int_env/2,
         get_float_env/1, get_float_env/2,
         get_binary_env/1, get_binary_env/2]).

get_env(Name) ->
  ?MODULE:get_env(Name, ?Undef).

get_env(Name, Def) ->
  convert(
    fun(Val) ->
      erlwater:to_binary(Val)
    end, Name, Def, "binary").

get_bool_env(Name) ->
  convert(
    fun(Val) ->
      erlwater:to_boolean(Val)
    end, Name, false, "boolean").

get_int_env(Name) ->
  ?MODULE:get_int_env(Name, ?Undef).

get_int_env(Name, Def) when is_integer(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_integer(Val)
    end, Name, Def, "integer").

get_float_env(Name) ->
  ?MODULE:get_float_env(Name, ?Undef).

get_float_env(Name, Def) when is_float(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_float(Val)
    end, Name, Def, "float").

get_binary_env(Name) ->
  ?MODULE:get_binary_env(Name, ?Undef).

get_binary_env(Name, Def) when is_binary(Def); Def == ?Undef ->
  convert(
    fun(Val) ->
      erlwater:to_binary(Val)
    end, Name, Def, "binary").

convert(ConvertFun, EnvName, Def, ExpectedType) ->
  Value = os:getenv(EnvName, Def),
  try
    ConvertFun(Value)
  catch
    _:_ ->
      throw_error(EnvName, ExpectedType, Value)
  end.

throw_error(EnvName, ExpectedType, Given) ->
  erlang:error(io:format("The env variable ~p must be an ~p but got ~p~n", [EnvName, ExpectedType, Given]),
               [EnvName, ExpectedType, Given]).