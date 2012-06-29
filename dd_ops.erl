%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(dd_ops).

-export([eq/2, leq/2, add/2, mul/2, sub/2]).

%%------------------------------------------------------------------------------
%% Operations
%%------------------------------------------------------------------------------

eq({const, A}, {const, B}) ->
  A == B.

leq({const, A}, {const, B}) ->
  A =< B.

add({const, A}, {const, B}) ->
  {const, A + B}.

mul({const, A}, {const, B}) ->
  {const, A * B}.

sub({const, A}, {const, B}) ->
  {const, A - B}.
