-module(erlwater).

-include("erlwater.hrl").

-export([round/2, safe_binary_to_term/1, to_integer/1, to_float/1, to_boolean/1, to_binary/1, term_to_base64/1, base64_to_term/1, tuple_to_binary/1]).


round(Number, Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.


safe_binary_to_term(B) when is_binary(B) ->
  erlang:binary_to_term(B, [safe]).


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
to_binary(T) ->
  term_to_binary(T).


tuple_to_binary(T) when is_tuple(T) ->
  iolist_to_binary(tuple_to_list(T)).


term_to_base64(Term) ->
  Bin = term_to_binary(Term),
  base64:encode(Bin).


base64_to_term(Data) ->
  case catch base64:decode(Data) of
    {'EXIT', _} -> error;
    Value ->
      case catch safe_binary_to_term(Value) of
        {'EXIT', _} -> error;
        Term -> Term
      end
  end.
