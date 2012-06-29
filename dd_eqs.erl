%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(dd_eqs).

-export([factorial_ast/0, fibonacci_ast/0]).

%%------------------------------------------------------------------------------
%% ASTs
%%------------------------------------------------------------------------------
%% The following code represents a set of equations translated to an AST. This
%% is so that we can present the semantic model without having to worry about
%% parsing its syntax. 
%%
%% Q represents a set of mutually recursive equations, and R represents the 
%% demands of the program.
%%
%% In order to make it clearer, the following is a high-level description which
%% could be parsed to the following ASTs.
%%
%% factorial_ast Q => 
%%   fact = if #0 == 0 then 1 else #0 * fact [0 <- #0 - 1]
%%
%% fibonnaci_ast Q =>
%%   fib = if #0 <= 1 then 0 else fib [0 <- #0 - 1] + fib [0 <- #0 - 2]
%%
%% factorial_ast R =>
%%   fact << 0 <- 12
%%
%% fibonacci_ast R =>
%%   fib << 0 <- 10
%%
%% Where # is the dimensional index operator, [A <- B] is the relative context 
%% operator, and Id << represents demands made to an identifier Id.
%%------------------------------------------------------------------------------

factorial_ast() ->
  Q = [{equation, {id, "fact"},
        {'if', {op, fun dd_ops:eq/2, [{dim, {const, 0}}, {const, 0}]},
           {const, 1},
           {op, fun dd_ops:mul/2, 
            [{dim, {const, 0}}, 
             {context_r, {id, "fact"}, 
              [{{const, 0}, 
                {op, fun dd_ops:sub/2, [{dim, {const, 0}}, {const, 1}]}}]}]}}}],
  R = {demand, {id, "fact"}, [{{const, 0}, {const, 12}}]},
  {Q, R}.

fibonacci_ast() ->
  Q = [{equation, {id, "fib"},
        {'if', {op, fun dd_ops:leq/2, [{dim, {const, 0}}, {const, 1}]},
         {dim, {const, 0}},
         {op, fun dd_ops:add/2, 
          [{context_r, {id, "fib"},
            [{{const, 0}, 
              {op, fun dd_ops:sub/2, [{dim, {const, 0}}, {const, 1}]}}]},
           {context_r, {id, "fib"},
            [{{const, 0}, 
              {op, fun dd_ops:sub/2, [{dim, {const, 0}}, {const, 2}]}}]}]}}}],
  R = {demand, {id, "fib"}, [{{const, 0}, {const, 10}}]},
  {Q, R}.

