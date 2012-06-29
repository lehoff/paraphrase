%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(dd_basic).

-export([test/0]).

%%------------------------------------------------------------------------------
%% R
%%------------------------------------------------------------------------------

r({demand, Id, Cs}, Q) ->
  evaluate(Id, Q, Cs).

%%------------------------------------------------------------------------------
%% The Evaluator
%%------------------------------------------------------------------------------

evaluate({const, Literal}, Q, C) ->
  {const, Literal};
evaluate({id, String}, Q, C) when is_list(String) ->
  case lists:keyfind({id, String}, 2, Q) of
    false  ->
      {error, undefined_identifier, String};
    {equation, _, Expr} ->
      evaluate(Expr, Q, C)
  end;
evaluate({op, F, Es}, Q, C) ->
  Vs = [evaluate(En, Q, C) || En <- Es],
  apply(F, Vs);
evaluate({'if', E1, E2, E3}, Q, C) ->
  case evaluate(E1, Q, C) of
    true  ->
      evaluate(E2, Q, C);
    false ->
      evaluate(E3, Q, C)
  end;
evaluate({dim, E} = Dim, Q, C) ->
  V = evaluate(E, Q, C),
  case lists:keyfind(V, 1, C) of
    false ->
      {error, undefined_dimension, V};
    {_, V1} ->
      V1
  end;
evaluate({context_r, E1, Es}, Q, C) ->
  Ce = [{evaluate(Vn1, Q, C), evaluate(Vn2, Q, C)} || {Vn1, Vn2} <- Es],
  evaluate(E1, Q, Ce).

%%------------------------------------------------------------------------------
%% An amazingly basic test
%%------------------------------------------------------------------------------

test() ->
  {Q, R} = dd_eqs:fibonacci_ast(),
  T1 = erlang:now(),
  P = r(R, Q),
  T2 = erlang:now(),
  {timer:now_diff(T2, T1), P}.
 
