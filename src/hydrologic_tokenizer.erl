% @hidden
-module(hydrologic_tokenizer).
-include("../include/hydrologic_tokens.hrl").

-export([tokenize/1]).

tokenize(String) ->
  tokenize(String, 1, 1, [], true).

tokenize([], Line, Column, Tokens, _NL) ->
  {ok, Line, Column, lists:reverse(Tokens)};

% Indent and space
tokenize([H|Rest], Line, Column, Tokens, NL) when ?is_cr(H) ->
  tokenize(Rest, Line, Column + 1, Tokens, NL);
tokenize([H|Rest], Line, _Column, Tokens, _NL) when ?is_lf(H) ->
  tokenize(Rest, Line + 1, 1, Tokens, true);
tokenize([H|Rest], Line, Column, Tokens, NL) when ?is_space(H) ->
  tokenize(Rest, Line, Column + 1, Tokens, NL);

% IN
tokenize([H|_] = String, Line, Column, Tokens, _NL) when ?is_in_out(H) ->
  {Rest, [$:|Identifier], Size, _} = build_identifier(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{in, {Line, Column, Column + Size}, Identifier}|Tokens], false);

% Integer and floats
tokenize([H|_] = String, Line, Column, Tokens, _NL) when ?is_digit(H) ->
  {Rest, Number, Size, Type} = build_number(String, [], integer),
  tokenize(Rest, Line, Column + Size, [{Type, {Line, Column, Column + Size}, Number}|Tokens], false);

% Identifier and keywords
tokenize([H|_] = String, Line, Column, Tokens, _NL) when ?is_identifier(H) ->
  {Rest, Identifier, Size, Type} = build_identifier(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{Type, {Line, Column, Column + Size}, Identifier}|Tokens], false);

tokenize([H|_] = String, Line, Column, Tokens, _NL) when ?is_op(H) ->
  case build_operator(String, 0, []) of
    {Rest, "|", Size} ->
      tokenize(Rest, Line, Column + Size, [{pipe, {Line, Column, Column + Size}, "|"}|Tokens], false);
    {Rest, "?", Size} ->
      tokenize(Rest, Line, Column + Size, [{return, {Line, Column, Column + Size}, "?"}|Tokens], false);
    {Rest, Operator, Size} ->
      tokenize(Rest, Line, Column + Size, [{operator, {Line, Column, Column + Size}, Operator}|Tokens], false)
  end.

% Private

build_number([$., N|R], Acc, integer) when ?is_digit(N) ->
  build_number([N|R], [$.|Acc], float);
build_number([N|R], Acc, Number) when ?is_digit(N) ->
  build_number(R, [N|Acc], Number);
build_number(Rest, Acc, float) ->
  {Rest, list_to_float(lists:reverse(Acc)), length(Acc), float};
build_number(Rest, Acc, integer) ->
  {Rest, list_to_integer(lists:reverse(Acc)), length(Acc), integer}.

build_identifier([H|Rest], Len, Current) when ?is_identifier_part(H) ->
  build_identifier(Rest, Len + 1, [H|Current]);
build_identifier(Rest, Len, Current) ->
  case Current of
    [$:|Out] ->
      {Rest, lists:reverse(Out), Len, out};
    _ ->
      Identifier = lists:reverse(Current),
      case erlang:length(string:tokens(Identifier, ":")) of
        2 ->
          {Rest, Identifier, Len, mf};
        _ ->
          {Rest, Identifier, Len, reserved_word(Identifier)}
      end
  end.

build_operator([H|Rest], Len, Current) when ?is_op(H) ->
  build_operator(Rest, Len + 1, [H|Current]);
build_operator(Rest, Len, Current) ->
  {Rest, lists:reverse(Current), Len}.

% Reserved

reserved_word("duplicate") -> duplicate;
reserved_word("merge") -> merge;
reserved_word("fanin") -> fanin;
reserved_word("fun") -> 'fun';
reserved_word("end") -> 'end';
reserved_word(_) -> identifier.
