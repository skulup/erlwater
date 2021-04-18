%%%-------------------------------------------------------------------
%%% @author kaja
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2021 7:02 AM
%%%-------------------------------------------------------------------
-module(ew_convert).
-author("kaja").

-include("erlwater.hrl").

%% API
-export([to_integer/1, to_float/1, to_boolean/1, to_binary/1, to_base64/1, base64_to_term/1, safe_binary_to_term/1]).

to_integer(?Undef) ->
  ?Undef;
to_integer(I) when is_integer(I) ->
  I;
to_integer(B) when is_binary(B) ->
  binary_to_integer(B);
to_integer(S) when is_list(S) ->
  list_to_integer(S).


to_float(?Undef) ->
  ?Undef;
to_float(F) when is_float(F) ->
  F;
to_float(B) when is_binary(B) ->
  B2 =
  case binary:match(B, [<<$.>>]) of
    nomatch ->
      <<B/binary, $., $0>>;
    _ ->
      B
  end,
  binary_to_float(B2);
to_float(S) when is_list(S) ->
  list_to_float(S).


to_boolean(B) when is_boolean(B) ->
  B;
to_boolean(?Undef) ->
  false;
to_boolean(N) when is_number(N) ->
  N > 0;
to_boolean(<<"0">>) ->
  false;
to_boolean(<<"1">>) ->
  true;
to_boolean("0") ->
  false;
to_boolean("1") ->
  true.


to_binary(B) when is_binary(B) ->
  B;
to_binary(S) when is_list(S) ->
  list_to_binary(S);
to_binary(I) when is_integer(I) ->
  integer_to_binary(I);
to_binary(F) when is_float(F) ->
  float_to_binary(F);
to_binary(A) when is_atom(A) ->
  atom_to_binary(A, utf8);
to_binary(T) when is_tuple(T) ->
  iolist_to_binary(tuple_to_list(T));
to_binary(T) ->
  term_to_binary(T).

to_base64(Term) ->
  Bin = ?MODULE:to_binary(Term),
  base64:encode(Bin).

base64_to_term(Data) ->
  case catch base64:decode(Data) of
    {'EXIT', _} -> error;
    Value ->
      case catch ?MODULE:safe_binary_to_term(Value) of
        {'EXIT', _} -> error;
        Term -> Term
      end
  end.

safe_binary_to_term(B) when is_binary(B) ->
  erlang:binary_to_term(B, [safe]).
