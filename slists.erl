%%------------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(slists).

-export([union/1, union/2]).

union(L) when is_list(L) ->
  lists:foldl(fun (L1, L2) -> union(L1, L2) end, [], L).

union(L1, L2) when is_list(L1), is_list(L2) ->                  
  ordsets:to_list(ordsets:union(ordsets:from_list(L1), ordsets:from_list(L2))).

  
