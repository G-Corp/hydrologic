-module(hydrologic_core_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_core_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        hydrologic:new(
          test,
          [
           fun(X) -> 2*X end, % 2 4 6
           {duplicate, a},
           fun(X) -> 3*X end, % 6 12 18
           {b, {merge, fun(X1, X2) ->
                           X1 + X2 % 10 20 30
                       end}},
           return,
           {a, fun(X) -> % 4 8 12
                   X + X
               end},
           b
          ]
         ),
        ?assertEqual({ok, 10}, hydrologic:run(test, 1)),
        ?assertEqual({ok, 20}, hydrologic:run(test, 2)),
        ?assertEqual({ok, 30}, hydrologic:run(test, 3)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {duplicate, a},
           {b, {merge, fun(X1, X2) ->
                           X1 + X2
                       end}},
           return,
           {a, fun(X) ->
                   {map, X * 10}
               end},
           b
          ]
         ),
        ?assertEqual({ok, [11, 22, 33]}, hydrologic:run(test, [1, 2, 3])),
        ?assertEqual({ok, 11}, hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {odd, a},
           {b, fanin},
           return,
           {a, fun(X) ->
                   {map, X * 10}
               end},
           b
          ]
         ),
        ?assertEqual({ok, [1, 20, 3]}, hydrologic:run(test, [1, 2, 3])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {duplicate, a},
           odd,
           {b, fanin},
           return,
           {a, fun(X) ->
                   {map, X * 10}
               end},
           b
          ]
         ),
        ?assertEqual({ok, [1, 20, 3]}, hydrologic:run(test, [1, 2, 3])),
        hydrologic:stop(test),

        hydrologic:new(
          test,
          [
           {duplicate, a},
           odd,
           {b, {fanin, 0}},
           return,
           {a, fun(X) ->
                   {map, X * 10}
               end},
           b
          ]
         ),
        ?assertEqual({ok, [1, 20, 3]}, hydrologic:run(test, [1, 2, 3])),
        hydrologic:stop(test),

        hydrologic:new(
          test,
          [
           {duplicate, a},
           odd,
           {b, {fanin, 1}},
           return,
           {a, fun(X) ->
                   {map, X * 10}
               end},
           b
          ]
         ),
        ?assertEqual({ok, [10, 20, 30]}, hydrologic:run(test, [1, 2, 3])),
        hydrologic:stop(test)
    end
   ]}.
