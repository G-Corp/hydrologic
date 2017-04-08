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
                                    % ?debugFmt(Format, Data)
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
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    % ?debugFmt(Format, Data)
                                    ok
                                end),
        hydrologic:new(test, [console]),
        Result = hydrologic:run(test, [10, 20, 30]),
        ?assertEqual({ok, [10, 20, 30]}, Result),
        ?assertCall(io, format, 2, 3),
        hydrologic:stop(test),
        meck:unload(io)
    end,
    fun() ->
        meck:new(io, [passthrough, unstick]),
        meck:expect(io, format, fun(_Format, _Data) ->
                                    % ?debugFmt(Format, Data)
                                    ok
                                end),
        hydrologic:new(test, [console]),
        Result = hydrologic:run(test, []),
        ?assertEqual({ok, []}, Result),
        ?assertCall(io, format, 2, 1), % TODO BUG ?assertCall(io, format, 2, 0),
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
        ?assertEqual({ok, ["world"]},
                     hydrologic:run(test, ["hello", "world"])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
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
           {{match, ["world"]}, a},
           fun string:to_upper/1,
           {a, fanin},
           return
          ]
         ),
        ?assertEqual(
           {ok, ["hello", "WORLD", "of", "love"]},
           hydrologic:run(test, ["hello", "world", "of", "love"])),
        hydrologic:stop(test)
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
        ?assertEqual({ok, ["hello.....", <<"world.....">>, 'love......']},
                     hydrologic:run(test, ["hello", <<"world">>, love])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
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
        ?assertEqual({ok, ["hello.....", <<"world.....">>, 'love......']},
                     hydrologic:run(test, ["hello", <<"world">>, love])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
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
        ?assertEqual({ok, [".....hello", <<".....world">>, '......love']},
                     hydrologic:run(test, ["hello", <<"world">>, love])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
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
        ?assertEqual({ok, ["hello w", <<"hello w">>, 'hello w']},
                     hydrologic:run(test, ["hello world", <<"hello world">>, 'hello world'])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [even]),
        ?assertEqual({ok, [2, 4]},
                     hydrologic:run(test, [1, 2, 3, 4])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 3, 5])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, 2)),
        ?assertEqual(ok,
                     hydrologic:run(test, 1)),
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
        ?assertEqual({ok, []},
                     hydrologic:run(test, [2, 4, 6])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, 3},
                     hydrologic:run(test, 3)),
        ?assertEqual(ok,
                     hydrologic:run(test, 2)),
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
        ?assertEqual({ok, 0},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, 4},
                     hydrologic:run(test, 4)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [sort]
         ),
        ?assertEqual({ok, [1, 2, 3, 4]},
                     hydrologic:run(test, [4, 2, 1, 3])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [4]},
                     hydrologic:run(test, 4)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{count, [chars]}]
         ),
        ?assertEqual({ok, 21},
                     hydrologic:run(test, ["hello\nworld", "hola mundo"])),
        ?assertEqual({ok, 20},
                     hydrologic:run(test, ["hello\nworld", <<"hola">>, 'mundo'])),
        ?assertEqual({ok, 11},
                     hydrologic:run(test, "hello\nworld")),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, <<"">>)),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, '')),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, "")),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{join, [", "]}]
         ),
        ?assertEqual({ok, "hello, world, love"},
                     hydrologic:run(test, ["hello", "world", "love"])),
        ?assertEqual({ok, "hello"},
                     hydrologic:run(test, "hello")),
        ?assertEqual({ok, ""},
                     hydrologic:run(test, [])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{count, [lines]}]
         ),
        ?assertEqual({ok, 3},
                     hydrologic:run(test, ["hello\nworld", "hola mundo"])),
        ?assertEqual({ok, 4},
                     hydrologic:run(test, ["hello\nworld", <<"hola">>, 'mundo'])),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, "hello\nworld")),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, <<"hello\nworld">>)),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, 'hello\nworld')),
        ?assertEqual({ok, 1},
                     hydrologic:run(test, "hello")),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, <<"">>)),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, '')),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, "")),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{count, [words]}]
         ),
        ?assertEqual({ok, 4},
                     hydrologic:run(test, ["hello\nworld", "hola mundo"])),
        ?assertEqual({ok, 4},
                     hydrologic:run(test, ["hello\nworld", <<"hola">>, 'mundo'])),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, "hello\nworld")),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, "hello world")),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, <<"hello world">>)),
        ?assertEqual({ok, 2},
                     hydrologic:run(test, 'hello world')),
        ?assertEqual({ok, 1},
                     hydrologic:run(test, "hello")),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, <<"">>)),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, '')),
        ?assertEqual({ok, 0},
                     hydrologic:run(test, "")),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{from, [3]}]
         ),
        ?assertEqual({ok, [3, 4, 5]},
                     hydrologic:run(test, [1, 2, 3, 4, 5])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [3]},
                     hydrologic:run(test, 3)),
        ?assertEqual({ok, []},
                     hydrologic:run(test, 4)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{to, [3]}]
         ),
        ?assertEqual({ok, [1, 2, 3]},
                     hydrologic:run(test, [1, 2, 3, 4, 5])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [3]},
                     hydrologic:run(test, 3)),
        ?assertEqual({ok, [4]},
                     hydrologic:run(test, 4)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [unique]
         ),
        ?assertEqual({ok, [1, 2, 3]},
                     hydrologic:run(test, [1, 1, 2, 2, 3, 1, 2, 3, 3, 3])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{head, [3]}]
         ),
        ?assertEqual({ok, [1, 2, 3]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7, 8, 9])),
        ?assertEqual({ok, [1, 2]},
                     hydrologic:run(test, [1, 2])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{head, [0]}]
         ),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7, 8, 9])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{tail, [3]}]
         ),
        ?assertEqual({ok, [7, 8, 9]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7, 8, 9])),
        ?assertEqual({ok, [1, 2]},
                     hydrologic:run(test, [1, 2])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{tail, [0]}]
         ),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7, 8, 9])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{drop, [head, 3]}]
         ),
        ?assertEqual({ok, [4, 5, 6]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2, 3])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]}, % TODO BUG must be {ok, []}
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{drop, [tail, 3]}]
         ),
        ?assertEqual({ok, [1, 2, 3]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2, 3])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [1, 2])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]}, % TODO BUG must be {ok, []}
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [flatten]
         ),
        ?assertEqual({ok, [1, 2, 3, 4, 5, 6, 7]},
                     hydrologic:run(test, [1, [2, 3], [4, [5, [6, 7]]]])),
        ?assertEqual({ok, [1, 2, 3, 4, 5, 6, 7]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{repeat, [3]}]
         ),
        ?assertEqual({ok, [a, a, a, b, b, b, c, c, c]},
                     hydrologic:run(test, [a, b, c])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{repeat, [3]}]
         ),
        ?assertEqual({ok, [a, a, a]},
                     hydrologic:run(test, a)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {repeat, [3]},
           unique
          ]
         ),
        ?assertEqual({ok, [a]},
                     hydrologic:run(test, a)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [unique]
         ),
        ?assertEqual({ok, [1, 2, 3]},
                     hydrologic:run(test, [1, 1, 2, 2, 3, 3, 1, 2, 3])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        ?assertEqual({ok, [1]},
                     hydrologic:run(test, 1)),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [{between, [3, 5]}]
         ),
        ?assertEqual(ok,
                     hydrologic:run(test, 2)),
        ?assertEqual({ok, 3},
                     hydrologic:run(test, 3)),
        ?assertEqual({ok, 4},
                     hydrologic:run(test, 4)),
        ?assertEqual({ok, 5},
                     hydrologic:run(test, 5)),
        ?assertEqual(ok,
                     hydrologic:run(test, 6)),
        ?assertEqual({ok, [3, 4, 5]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7])),
        ?assertEqual({ok, []},
                     hydrologic:run(test, [])),
        hydrologic:stop(test)
    end,
    fun() ->
        hydrologic:new(
          test,
          [
           {{between, [3, 5]}, a},
           fun(X) -> X * X end,
           {a, fanin}
          ]
         ),
        ?assertEqual({ok, [1, 2, 9, 16, 25, 6, 7]},
                     hydrologic:run(test, [1, 2, 3, 4, 5, 6, 7])),
        % TODO BUG ?assertEqual({ok, []},
        % TODO BUG              hydrologic:run(test, [])),
        hydrologic:stop(test)
    end
   ]}.
