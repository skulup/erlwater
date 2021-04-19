-module(erlwater).

-include("erlwater.hrl").

-export([round/2]).
-export([trim/1]).
-export([to_integer/1, to_float/1, to_boolean/1, to_binary/1, to_base64/1, base64_to_term/1, safe_binary_to_term/1]).
-export([assert/3, is_boolean/1, is_number/1, is_integer/1, is_string/1, is_proplist/1, is_positive_int/1, is_non_negative_int/1]).
-export([get_env/1, get_bool_env/1, get_int_env/1, get_int_env/2, get_float_env/1, get_float_env/2, get_binary_env/1, get_binary_env/2]).
-export([get_bool_app_env/2, get_int_app_env/2, get_int_app_env/3, get_float_app_env/2, get_float_app_env/3, get_binary_app_env/2, get_binary_app_env/3]).

%% Mathematics

round(Number, Precision) ->
  ew_math:round(Number, Precision).

%% Conversions

to_integer(V) ->
  ew_convert:to_integer(V).

to_float(V) ->
  ew_convert:to_float(V).

to_boolean(V) ->
  ew_convert:to_boolean(V).

to_binary(V) ->
  ew_convert:to_binary(V).

to_base64(V) ->
  ew_convert:to_base64(V).

base64_to_term(V) ->
  ew_convert:base64_to_term(V).

safe_binary_to_term(B) ->
  ew_convert:safe_binary_to_term(B).

%% Strings
trim(S) ->
  ew_string:trim(S).

%% Applications Environment Variables

get_bool_app_env(App, Name) ->
  ew_app:get_bool_env(App, Name).

get_int_app_env(App, Name) ->
  ew_app:get_int_env(App, Name).

get_int_app_env(App, Name, Def) ->
  ew_app:get_int_env(App, Name, Def).

get_float_app_env(App, Name) ->
  ew_app:get_float_env(App, Name).

get_float_app_env(App, Name, Def) ->
  ew_app:get_float_env(App, Name, Def).

get_binary_app_env(App, Name) ->
  ew_app:get_binary_env(App, Name).

get_binary_app_env(App, Name, Def) ->
  ew_app:get_binary_env(App, Name, Def).

%% OS Environment Variables

get_env(Name) ->
  ew_os:get_env(Name).

get_bool_env(Name) ->
  ew_os:get_bool_env(Name).

get_int_env(Name) ->
  ew_os:get_int_env(Name).

get_int_env(Name, Def) ->
  ew_os:get_int_env(Name, Def).

get_float_env(Name) ->
  ew_os:get_float_env(Name).

get_float_env(Name, Def) ->
  ew_os:get_float_env(Name, Def).

get_binary_env(Name) ->
  ew_os:get_binary_env(Name).

get_binary_env(Name, Def) ->
  ew_os:get_binary_env(Name, Def).

%% Assertions
assert(Expression, Arg, Error) ->
  ew_assertions:assert(Expression, Arg, Error).

is_boolean(V) ->
  ew_assertions:is_boolean(V).

is_number(V) ->
  ew_assertions:is_number(V).

is_integer(V) ->
  ew_assertions:is_number(V).

is_positive_int(V) ->
  ew_assertions:is_positive_int(V).

is_non_negative_int(V) ->
  ew_assertions:is_non_negative_int(V).

is_string(V) ->
  ew_assertions:is_string(V).

is_proplist(V) ->
  ew_assertions:is_proplist(V).
