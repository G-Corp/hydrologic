-module(hydrologic_stdlib).
-include_lib("../include/hydrologic_lib.hrl").

-export([
         return/1
         , console/1
         , console/2
        ]).

-export([
         match/2
         , pad/3
         , pad/4
         , chop/2
         , between/3
        ]).

-export([
         odd/1
         , even/1
         , sum/1, sum/2
        ]).

% Common

-spec return(any()) -> {return, any()}.
return(Data) ->
  {return, Data}.

-spec console(any()) -> {map, any()}.
console(Data) ->
  console(Data, "~p~n").

-spec console(any(), string()) -> {map, any()}.
console(Data, Format) ->
  io:format(Format, [Data]),
  {map, Data}.

% String

-spec match(any(), string() | binary()) -> {filter, boolean()}.
match(Data, Regex) ->
  case re:run(bucs:to_string(Data), bucs:to_string(Regex), [global]) of
    {match, _} ->
      {filter, true};
    _ ->
      {filter, false}
  end.

-spec pad(any(), non_neg_integer(), integer()) -> {map, any()}.
pad(Data, Size, Char) ->
  pad(Data, right, Size, Char).

-spec pad(any(), right | left, non_neg_integer(), integer()) -> {map, any()}.
pad(Data, right, Size, Char) ->
  {map, bucs:as(Data, string:left(bucs:to_string(Data), Size, Char))};
pad(Data, left, Size, Char) ->
  {map, bucs:as(Data, string:right(bucs:to_string(Data), Size, Char))}.

-spec chop(any(), non_neg_integer()) -> {map, any()}.
chop(Data, Size) ->
  {map, bucs:as(Data, string:sub_string(bucs:to_string(Data), 1, Size))}.

-spec between(any(), any(), any()) -> {filter, boolean()}.
between(Data, Min, Max) ->
  {filter,
   bucs:to_string(Data) =< bucs:to_string(Max) andalso
   bucs:to_string(Data) >= bucs:to_string(Min)}.

% Integer

-spec even(integer()) -> {filter, boolean()}.
even(Data) ->
  {filter, Data rem 2 == 0}.

-spec odd(integer()) -> {filter, boolean()}.
odd(Data) ->
  {filter, Data rem 2 /= 0}.

-spec sum(integer()) -> {reduce, hydrologic:accumulator()}.
sum(Data) ->
  {reduce, Data}.
-spec sum(integer(), hydrologic:accumulator()) -> {reduce, hydrologic:accumulator()}.
sum(Data, Acc) ->
  {reduce, Acc + Data}.
