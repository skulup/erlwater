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
-export([get_env/2]).


get_env(Env, Default) ->
  case application:get_env(Env) of
    undefined -> Default;
    {ok, undefined} -> Default;
    {ok, Value} -> Value
  end.