%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(dd_intermediate).

-export([test/0]).

%%------------------------------------------------------------------------------
%% R
%%------------------------------------------------------------------------------

r({demand, Id, Cs}, Q) ->
  evaluate(Id, Q, Cs, []).

%%------------------------------------------------------------------------------
%% The Evaluator
%%------------------------------------------------------------------------------

evaluate({const, Literal}, Q, C, D) ->
  {{const, Literal}, [], []};
evaluate({id, String} = Id1, Q, C, D) when is_list(String) ->
  case lists:keyfind(Id1, 2, Q) of
    false  ->
      {error, undefined_identifier, String};
    {equation, _, Expr} ->
      {V1, C1, D1} = evaluate(Expr, Q, C, D),
      {V1, C1, slists:union(D1, [{Id1, C1, V1}])}
  end;
evaluate({op, F, Es}, Q, C, D) ->
  VCDs = [evaluate(En, Q, C, D) || En <- Es],
  Vs = [X || {X, _, _} <- VCDs],
  Cs = [X || {_, X, _} <- VCDs],
  Ds = [X || {_, _, X} <- VCDs],
  {apply(F, Vs), slists:union(Cs), slists:union(Ds)};
evaluate({'if', E1, E2, E3}, Q, C, D) ->
  case evaluate(E1, Q, C, D) of
    {true, C1, D1} ->
      {V2, C2, D2} = evaluate(E2, Q, C1, D1),
      {V2, slists:union(C1, C2), slists:union(D1, D2)};
    {false, C1, D1} ->
      {V3, C3, D3} = evaluate(E3, Q, C1, D1),
      {V3, slists:union(C1, C3), slists:union(D1, D3)}
  end;
evaluate({dim, E}, Q, C, D) ->
  {V1, C1, D1} = evaluate(E, Q, C, D),
  case lists:keyfind(V1, 1, C) of
    false ->
      {error, undefined_dimension, V1};
    {_, CV} ->
      {CV, slists:union(C1, [{V1, CV}]), D1}
  end;
evaluate({context_r, E1, Es}, Q, C, D) ->
  VCDE1 = [{evaluate(Vn1, Q, C, D), evaluate(Vn2, Q, C, D)} || {Vn1, Vn2} <- Es],
  VCDE  = [{V1, V2} || {{V1, _, _}, {V2, _, _}} <- VCDE1],
  Cs = [X || {_, X, _} <- VCDE],
  Ds = [X || {_, _, X} <- VCDE],
  {V1, _, _} = evaluate(E1, Q, VCDE, D),
  {V1, slists:union(Cs), slists:union(Ds)}.

%%------------------------------------------------------------------------------
%% An amazingly basic test
%%------------------------------------------------------------------------------

test() ->
  {Q, R} = dd_eqs:fibonacci_ast(),
  T1 = erlang:now(),
  P = r(R, Q),
  T2 = erlang:now(),
  {timer:now_diff(T2, T1), P}.


 
