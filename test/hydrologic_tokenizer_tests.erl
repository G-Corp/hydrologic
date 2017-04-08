-module(hydrologic_tokenizer_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_tokenizer_tests_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assertMatch(
           {ok, 1, 4, [{integer, {1, 1, 4}, 123}]},
           hydrologic_tokenizer:tokenize("123")),
        ?assertMatch(
           {ok, 1, 7, [{float, {1, 1, 7}, 123.45}]},
           hydrologic_tokenizer:tokenize("123.45")),
        ?assertMatch(
           {ok, 1, 10, [{integer, {1, 3, 6}, 123},
                        {integer, {1, 7, 10}, 456}]},
           hydrologic_tokenizer:tokenize("  123 456")),
        ?assertMatch(
           {ok, 1, 13, [{float, {1, 3, 9}, 123.45},
                        {integer, {1, 10, 13}, 678}]},
           hydrologic_tokenizer:tokenize("  123.45 678"))
    end,
    fun() ->
        ?assertMatch(
           {ok, 1, 5, [{identifier, {1, 1, 5}, "toto"}]},
           hydrologic_tokenizer:tokenize("toto")),
        ?assertMatch(
           {ok, 1, 7, [{identifier, {1, 3, 7}, "toto"}]},
           hydrologic_tokenizer:tokenize("  toto")),
        ?assertMatch(
           {ok, 1, 11, [{identifier, {1, 3, 7}, "toto"},
                        {integer, {1, 8, 11}, 123}]},
           hydrologic_tokenizer:tokenize("  toto 123")),
        ?assertMatch(
           {ok, 1, 18, [{identifier, {1, 3, 8}, "match"},
                        {identifier, {1, 9, 13}, "toto"},
                        {identifier, {1, 14, 18}, "with"}]},
           hydrologic_tokenizer:tokenize("  match toto with"))
    end,
    fun() ->
      ?assertMatch(
         {ok, 1, 16, [{mf, {1, 1, 16}, "module:function"}]},
         hydrologic_tokenizer:tokenize("module:function"))
    end,
    fun() ->
        ?assertMatch(
           {ok, 1, 37,
            [{pipe, {1, 1, 2}, "|"},
             {operator, {1, 3, 4}, "<"},
             {identifier, {1, 5, 9}, "file"},
             {identifier, {1, 10, 13}, "txt"},
             {pipe, {1, 14, 15}, "|"},
             {identifier, {1, 16, 21}, "xlate"},
             {identifier, {1, 22, 27}, "upper"},
             {pipe, {1, 28, 29}, "|"},
             {identifier, {1, 30, 37}, "console"}]},
           hydrologic_tokenizer:tokenize("| < file txt | xlate upper | console"))
    end
   ]}.
