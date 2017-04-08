-module(hydrologic_parser_tests).
-include_lib("eunit/include/eunit.hrl").

hydrologic_parser_tests_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    % Identifier and return
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| console"),
        ?assertEqual(
           {ok, "[console]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| even | console"),
        ?assertEqual(
           {ok, "[even, console]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("?"),
        ?assertEqual(
           {ok, "[return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| console ?"),
        ?assertEqual(
           {ok, "[console, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| even | console ?"),
        ?assertEqual(
           {ok, "[even, console, return]"},
           hydrologic_parser:parse(Tokens))
    end,

    % IN OUT
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a ?"),
        ?assertEqual(
           {ok, "[a, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("? :a"),
        ?assertEqual(
           {ok, "[{a, return}]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a even ?"),
        ?assertEqual(
           {ok, "[{a, even}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| even b: ?"),
        ?assertEqual(
           {ok, "[{even, b}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a even b: ?"),
        ?assertEqual(
           {ok, "[{a, {even, b}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,

    % Function
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| fun module:function/1 ?"),
        ?assertEqual(
           {ok, "[fun module:function/1, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fun module:function/1 ?"),
        ?assertEqual(
           {ok, "[{a, fun module:function/1}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| fun module:function/1 b: ?"),
        ?assertEqual(
           {ok, "[{fun module:function/1, b}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fun module:function/1 b: ?"),
        ?assertEqual(
           {ok, "[{a, {fun module:function/1, b}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| fun(X) -> X + 1 end ?"),
        ?assertEqual(
           {ok, "[fun(X) -> X+1 end, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fun(X) -> X + 1 end ?"),
        ?assertEqual(
           {ok, "[{a, fun(X) -> X+1 end}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| fun(X) -> X + 1 end b: ?"),
        ?assertEqual(
           {ok, "[{fun(X) -> X+1 end, b}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fun(X) -> X + 1 end b: ?"),
        ?assertEqual(
           {ok, "[{a, {fun(X) -> X+1 end, b}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,

    % fanin
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| fanin ?"),
        ?assertMatch(
           {error, _},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fanin ?"),
        ?assertEqual(
           {ok, "[{a, fanin}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fanin 0 ?"),
        ?assertEqual(
           {ok, "[{a, {fanin, 0}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a fanin 1 ?"),
        ?assertEqual(
           {ok, "[{a, {fanin, 1}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,

    % merge
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| merge fun(X, Y) -> X + Y end ?"),
        ?assertEqual(
           {ok, "[{merge, fun(X, Y) -> X+Y end}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a merge fun(X, Y) -> X + Y end ?"),
        ?assertEqual(
           {ok, "[{a, {merge, fun(X, Y) -> X+Y end}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| merge fun module:function/2 ?"),
        ?assertEqual(
           {ok, "[{merge, fun module:function/2}, return]"},
           hydrologic_parser:parse(Tokens))
    end,
    fun() ->
        {ok, _, _, Tokens} = hydrologic_tokenizer:tokenize("| :a merge fun module:function/2 ?"),
        ?assertEqual(
           {ok, "[{a, {merge, fun module:function/2}}, return]"},
           hydrologic_parser:parse(Tokens))
    end,


    fun() ->
        ?assert(true)
        %{ok, _, _, R} =
        %hydrologic_tokenizer:tokenize("| fun(X) -> 2 * X end\n" ++
        %                              "| fun(X) -> \"he\\\"llo\" ++ X end\n" ++
        %                              "| duplicate a:\n" ++
        %                              "| fun(X) -> 3 * X end\n" ++
        %                              "| :b merge fun(X1, X2) -> X1 + X2 end\n" ++
        %                              "?\n" ++
        %                              "| :a fun(X) -> X + X || 2 end\n" ++
        %                              "| b:" ++
        %                              ""),
        %X = hydrologic_parser:parse(R),
        %?debugFmt("-----~n~p~n~p~n-----", [R, X]),

        % {ok, _, _, R0} = hydrologic_tokenizer:tokenize("| hello ?"),
        % X0 = hydrologic_parser:parse(R0),
        % ?debugFmt("-----~n~p~n~p~n-----", [R0, X0]),

        % {ok, _, _, R1} = hydrologic_tokenizer:tokenize("| hello"),
        % X1 = hydrologic_parser:parse(R1),
        % ?debugFmt("-----~n~p~n~p~n-----", [R1, X1]),

        % {ok, _, _, R2} = hydrologic_tokenizer:tokenize("| hello | world"),
        % X2 = hydrologic_parser:parse(R2),
        % ?debugFmt("-----~n~p~n~p~n-----", [R2, X2]),

        % {ok, _, _, R3} = hydrologic_tokenizer:tokenize("| hello | beautiful world"),
        % X3 = hydrologic_parser:parse(R3),
        % ?debugFmt("-----~n~p~n~p~n-----", [R3, X3]),

        % {ok, _, _, R4} = hydrologic_tokenizer:tokenize("| fun(X) -> {map, X*X} end ?"),
        % X4 = hydrologic_parser:parse(R4),
        % ?debugFmt("-----~n~p~n~p~n-----", [R4, X4]),

        % {ok, _, _, R5} = hydrologic_tokenizer:tokenize("| :b fun(X) -> {map, X*X} end ?"),
        % X5 = hydrologic_parser:parse(R5),
        % ?debugFmt("-----~n~p~n~p~n-----", [R5, X5]),

        % {ok, _, _, R6} = hydrologic_tokenizer:tokenize("| :b fun(X) -> {map, X*X} end c: ?"),
        % X6 = hydrologic_parser:parse(R6),
        % ?debugFmt("-----~n~p~n~p~n-----", [R6, X6])
    end
   ]}.
