%%%-------------------------------------------------------------------
%%% @author Alpha Umaru Shaw <shawalpha5@gmail.com>
%%% @doc
%%%
%%% @end
%%% Company: Skulup Ltd
%%% Copyright: (C) 2019
%%%-------------------------------------------------------------------
-module(erlwater_collection).
-author("Alpha Umaru Shaw").

%% API
-export([random_select/1, consistent_select/2, proplist_flattens/1, proplist_to_map/1,
  proplist_keys/1, contiguous_items_to_proplist/1, to_proplist/2]).

proplist_keys(Ls) ->
  proplist_keys(Ls, []).

contiguous_items_to_proplist(Ls) ->
  contiguous_items_to_proplist(Ls, []).
contiguous_items_to_proplist([], Ls) ->
  Ls;
contiguous_items_to_proplist([K, V | R], Ls) ->
  contiguous_items_to_proplist(R, [{K, V} | Ls]).


to_proplist(Keys, Vals) ->
  to_proplist(Keys, Vals, []).

to_proplist([], [], Ls) ->
  Ls;
to_proplist([K | Ks], [V | Vs], Ls) ->
  to_proplist(Ks, Vs, [{K, V} | Ls]).


proplist_keys([], Acc) ->
  Acc;
proplist_keys([{Key, _} | R], Acc) ->
  proplist_keys(R, [Key | Acc]).


proplist_flattens(Ls) ->
  proplist_flattens(Ls, []).

proplist_flattens([], Acc) ->
  Acc;
proplist_flattens([{Key, Value} | Rest], Acc) ->
  proplist_flattens(Rest, [Key, Value | Acc]).

proplist_to_map(Ls) ->
  proplist_to_map(Ls, #{}).


proplist_to_map([{K, V} | R], Map) ->
  proplist_to_map(R, maps:put(K, V, Map)).


consistent_select(Seed, Ls) ->
  seed_select(Seed, Ls).


random_select([]) ->
  false;
random_select([I]) ->
  I;
random_select(Ls) ->
  seed_select(erlwater_time:microseconds(), Ls).

seed_select(Seed, Ls) ->
  I = erlang:phash2(Seed, length(Ls)),
  lists:nth(I + 1, Ls).