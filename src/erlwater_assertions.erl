%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2020
%%%-------------------------------------------------------------------
-module(erlwater_assertions).
-author("Alpha Umaru Shaw").

%% API
-export([assert/3, is_boolean/1, is_number/1, is_integer/1, is_string/1, is_proplist/1, is_positive_int/1, is_non_negative_int/1]).

-define(BOOLEAN, "boolean").
-define(NUMBER, "number").
-define(INTEGER, "integer").
-define(STRING, "binary or string").
-define(POS_INTEGER, "positive integer").
-define(NON_NEG_INTEGER, "non-negative integer").
-define(PROPLIST, "proplist").

assert(Expression, Arg, ErrorReason) ->
  if Expression ->
    Arg;
  true ->
    erlang:error(ErrorReason, [Arg])
  end.

is_boolean({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() -> erlang:is_boolean(Value) end, ?BOOLEAN);

is_boolean(Value) ->
  ?MODULE:is_boolean({Value, Value}),
  Value.

is_number({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() -> erlang:is_number(Value) end, ?NUMBER);

is_number(Value) ->
  ?MODULE:is_number({Value, Value}),
  Value.


is_integer({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() -> erlang:is_integer(Value) end, ?INTEGER);

is_integer(Value) ->
  ?MODULE:is_integer({Value, Value}),
  Value.

is_positive_int({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() ->
    erlang:is_integer(Value) andalso Value > 0 end, ?POS_INTEGER);

is_positive_int(Value) ->
  ?MODULE:is_positive_int({Value, Value}),
  Value.

is_non_negative_int({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() ->
    erlang:is_integer(Value) andalso Value >= 0 end, ?NON_NEG_INTEGER);

is_non_negative_int(Value) ->
  ?MODULE:is_non_negative_int({Value, Value}),
  Value.

is_string({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value, fun() ->
    is_binary(Value) or io_lib:printable_list(Value) end, ?STRING);

is_string(Value) ->
  ?MODULE:is_string({Value, Value}),
  Value.

is_proplist({Key, Value} = Arg) ->
  assert_type(Key, Arg, Value,
    fun() -> is_a_prop_list(Value) end, ?PROPLIST);

is_proplist(Value) ->
  ?MODULE:is_proplist({Value, Value}),
  Value.


assert_type(Prop, Arg, Given, Checker, Type) ->
  case Checker() of
    true ->
      Arg;
    _ ->
      bad_value(Prop, Given, Type, Arg)
  end.

is_a_prop_list([]) ->
  true;
is_a_prop_list(Ls) when is_list(Ls) ->
  lists:all(
    fun({_, _}) ->
      true;
      (_) ->
        false
    end, Ls);
is_a_prop_list(_) -> false.

bad_value(Prop, Given, Msg, Args) ->
  Msg2 = lists:flatten(io_lib:format("value of property: `~p` must be ~s. Given: ~p", [Prop, Msg, Given])),
  erlang:error({bad_value, Msg2}, [Args]).