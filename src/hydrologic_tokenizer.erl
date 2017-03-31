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
% tokenize([H|_] = String, Line, Column, Tokens, NL) when ?is_space(H), NL ->
%   {Rest, Size} = build_ident(String, 0),
%   tokenize(Rest, Line, Column + Size, Tokens, NL);
tokenize([H|Rest], Line, Column, Tokens, NL) when ?is_space(H) ->
  tokenize(Rest, Line, Column + 1, Tokens, NL);

% Pipe and return
% tokenize([H|Rest], Line, Column, Tokens, true) when ?is_pipe_op(H) ->
%   tokenize(Rest, Line, Column + 1, [{pipe, {Line, Column, Column + 1}}|Tokens], false);
% tokenize([H|Rest], Line, Column, Tokens, true) when ?is_return_op(H) ->
%   tokenize(Rest, Line, Column + 1, [{return, {Line, Column, Column + 1}}|Tokens], false);

% IN
tokenize([H|_] = String, Line, Column, Tokens, _NL) when ?is_in_out(H) ->
  {Rest, Identifier, Size, _} = build_identifier(String, 0, []),
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
  {Rest, Operator, Size} = build_operator(String, 0, []),
  tokenize(Rest, Line, Column + Size, [{operator, {Line, Column, Column + Size}, Operator}|Tokens], false).

% End of line
% tokenize("\n" ++ Rest, Line, Column, Tokens) ->
%   {Line1, Tokens1} = eol(Line, Column, Tokens),
%   tokenize(Rest, Line1, 1, Tokens1);
% tokenize("\r\n" ++ Rest, Line, Column, Tokens) ->
%   {Line1, Tokens1} = eol(Line, Column, Tokens),
%   tokenize(Rest, Line1, 1, Tokens1).

% Private

% build_ident([N|R], Size) when ?is_space(N) ->
%   build_ident(R, Size + 1);
% build_ident(Rest, Size) ->
%   {Rest, Size}.

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
  Identifier = list_to_atom(lists:reverse(Current)),
  case Current of
    [X|_] when ?is_in_out(X) ->
      {Rest, Identifier, Len, out};
    _ ->
      {Rest, Identifier, Len, reserved_word(Identifier)}
  end.

build_operator([H|Rest], Len, Current) when ?is_op(H) ->
  build_operator(Rest, Len + 1, [H|Current]);
build_operator(Rest, Len, Current) ->
  {Rest, lists:reverse(Current), Len}.


% eol(_Line, _Column, [{';', _}|_] = Tokens) -> Tokens;
% eol(_Line, _Column, [{',', _}|_] = Tokens) -> Tokens;
% eol(Line, _Column, [{eol, _}|_] = Tokens) -> {Line + 1, Tokens};
% eol(Line, Column, Tokens) -> {Line + 1, [{eol, {Line, Column, Column + 1}}|Tokens]}.

reserved_word('duplicate') -> duplicate;
reserved_word('merge') -> merge;
reserved_word('fanin') -> fanin;
reserved_word('fun') -> 'fun';
reserved_word('end') -> 'end';
reserved_word(_) -> identifier.
