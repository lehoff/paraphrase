%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(dd_eqs).

-export([factorial_ast/0, fibonacci_ast/0]).

%%------------------------------------------------------------------------------
%% ASTs
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

