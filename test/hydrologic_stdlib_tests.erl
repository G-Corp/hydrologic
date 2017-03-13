-module(hydrologic_stdlib_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

hydrologic_stdlib_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    %?debugFmt(Format, Data)
                                    ok
                                end),
        hydrologic:new(test, [console]),
        Result = hydrologic:run(test, 10),
        ?assertEqual({ok, 10}, Result),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(test),
        meck:unload(io)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           fun(X) ->
               {map, X * 2}
           end,
           return,
           fun(X) -> %% Will never be called
               {map, X / 2}
           end
          ]
         ),
        ?assertEqual({ok, 20}, hydrologic:run(test, 10)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {match, ["world"]}
          ]
         ),
        ?assertEqual({ok, "hello world"}, hydrologic:run(test, "hello world")),
        ?assertEqual({ok, "world of love"}, hydrologic:run(test, "world of love")),
        ?assertEqual({ok, "the world is beautiful"}, hydrologic:run(test, "the world is beautiful")),
        ?assertEqual({ok, <<"hello world">>}, hydrologic:run(test, <<"hello world">>)),
        ?assertEqual({ok, 'hello world'}, hydrologic:run(test, 'hello world')),
        ?assertEqual(ok, hydrologic:run(test, "hola mundo")),
        hydrologic:stop(test)
    end,
    fun() ->
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    %?debugFmt(Format, Data)
                                    ok
                                end),
        hydrologic:new(
          test,
          [
           {{match, ["world"]}, a},
           {b, return},
           {a, console},
           b
          ]
         ),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, "hello world"}, hydrologic:run(test, "hello world")),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, "hola mundo"}, hydrologic:run(test, "hola mundo")),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(test),
        meck:unload(io)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {pad, [10, $.]}
          ]
         ),
        ?assertEqual({ok, "hello....."}, hydrologic:run(test, "hello")),
        ?assertEqual({ok, <<"hello.....">>}, hydrologic:run(test, <<"hello">>)),
        ?assertEqual({ok, 'hello.....'}, hydrologic:run(test, 'hello')),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {pad, [right, 10, $.]}
          ]
         ),
        ?assertEqual({ok, "hello....."}, hydrologic:run(test, "hello")),
        ?assertEqual({ok, <<"hello.....">>}, hydrologic:run(test, <<"hello">>)),
        ?assertEqual({ok, 'hello.....'}, hydrologic:run(test, 'hello')),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {pad, [left, 10, $.]}
          ]
         ),
        ?assertEqual({ok, ".....hello"}, hydrologic:run(test, "hello")),
        ?assertEqual({ok, <<".....hello">>}, hydrologic:run(test, <<"hello">>)),
        ?assertEqual({ok, '.....hello'}, hydrologic:run(test, 'hello')),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {chop, [7]}
          ]
         ),
        ?assertEqual({ok, "hello w"}, hydrologic:run(test, "hello world")),
        ?assertEqual({ok, <<"hello w">>}, hydrologic:run(test, <<"hello world">>)),
        ?assertEqual({ok, 'hello w'}, hydrologic:run(test, 'hello world')),
        ?assertEqual({ok, "hello"}, hydrologic:run(test, "hello")),
        ?assertEqual({ok, <<"hello">>}, hydrologic:run(test, <<"hello">>)),
        ?assertEqual({ok, 'hello'}, hydrologic:run(test, 'hello')),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [even]),
        ?assertEqual({ok, [2, 4]},
                     hydrologic:run(test, [1, 2, 3, 4])),
        hydrologic:stop(test)
    end,
    fun() ->
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    % ?debugFmt(Format, Data),
                                    ok
                                end),
        hydrologic:new(
          test,
          [
           {even, a},
           {b, return},
           {a, console},
           b
          ]
         ),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, 2)),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, 3},
                     hydrologic:run(test, 3)),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(test),
        meck:unload(io)
    end,
    fun() ->
        hydrologic:new(
          test,
          [odd]),
        ?assertEqual({ok, [1, 3]},
                     hydrologic:run(test, [1, 2, 3, 4])),
        hydrologic:stop(test)
    end,
    fun() ->
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    % ?debugFmt(Format, Data),
                                    ok
                                end),
        hydrologic:new(
          test,
          [
           {odd, a},
           {b, return},
           {a, console},
           b
          ]
         ),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, 3},
                     hydrologic:run(test, 3)),
        ?assertCall(io, format, 2, 0),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, 2)),
        ?assertCall(io, format, 2, 1),
        hydrologic:stop(test),
        meck:unload(io)
    end,
    fun() ->
        hydrologic:new(
          test,
          [sum]
         ),
        ?assertEqual({ok, 1+2+3+4},
                     hydrologic:run(test, [1, 2, 3, 4])),
        hydrologic:stop(test)
    end
   ]}.
