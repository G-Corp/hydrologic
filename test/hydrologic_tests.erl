-module(hydrologic_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_test_() ->
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
           fun(X) ->
               X * 2
           end,
           fun(X) ->
               X + 2
           end
          ]
         ),
        ?assertEqual({ok, 22}, hydrologic:run(test, 10)),
        ?assertEqual({ok, 16}, hydrologic:run(test, 7)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {a, {fun(X) ->
                    {filter, X rem 2 == 0}
                end, b}},
           fun(X) ->
               {map, X * 2}
           end,
           {b, fun(X) ->
                   {map, X + 2}
               end},
           return
          ]
         ),
        ?assertEqual({ok, 22}, hydrologic:run(test, 10)),
        ?assertEqual({ok, 9}, hydrologic:run(test, 7)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {fun(X) ->
                {filter, X rem 2 == 0}
            end, a},
           fun(X) ->
               {map, X * 2}
           end,
           {b, fun(X) ->
                   {map, X + 2}
               end},
           return,
           {a, b}
          ]
         ),
        ?assertEqual({ok, 22}, hydrologic:run(test, 10)),
        ?assertEqual({ok, 9}, hydrologic:run(test, 7)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           fun
             (X) when X rem 2 == 0 ->
               X * 2;
             (_) ->
               {error, odd}
           end,
           fun(X) ->
               X + 2
           end
          ]
         ),
        ?assertEqual({ok, 22}, hydrologic:run(test, 10)),
        ?assertEqual({error, odd, 7}, hydrologic:run(test, 7)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           fun({'__end__', Acc}) -> {reduce, lists:reverse(Acc)};
              ({Data, Acc}) -> {reduce, [Data * Data | Acc]};
              (Data) -> {reduce, [Data]}
           end
          ]
         ),
        ?assertEqual({ok, [1, 4, 9, 16]},
                     hydrologic:run(test, [1, 2, 3, 4])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           fun({'__end__', Acc}) -> {reduce, Acc};
              ({Data, Acc}) -> {reduce, Acc + Data};
              (Data) -> {reduce, Data}
           end
          ]
         ),
        ?assertEqual({ok, 10},
                     hydrologic:run(test, [1, 2, 3, 4])),
        hydrologic:stop(test)
    end
   ]}.
