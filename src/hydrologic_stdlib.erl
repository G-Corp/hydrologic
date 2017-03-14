-module(hydrologic_stdlib).
-include_lib("../include/hydrologic_lib.hrl").

-export([
         return/1
         , console/1
         , console/2
         , sort/1, sort/2
         , from/2, from/3
         , to/2, to/3
         , unique/1, unique/2
         , head/2, head/3
         , tail/2, tail/3
         , drop/3, drop/4
         , flatten/1, flatten/2
        ]).

-export([
         match/2
         , pad/3
         , pad/4
         , chop/2
         , between/3
         , count/2, count/3
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

-spec sort(any()) -> {reduce, list()}.
sort(Data) ->
  {reduce, [Data]}.
-spec sort(any(), hydrologic:accumulator()) -> {reduce, hydrologic:accumulator()}.
sort('__end__', Acc) ->
  {reduce, lists:sort(Acc)};
sort(Data, Acc) ->
  {reduce, [Data|Acc]}.

-spec from(any(), any()) -> {reduce, hydrologic:accumulator()}.
from(Data, Data) ->
  {reduce, [Data]};
from(_, _) ->
  {reduce, []}.
-spec from(any(), hydrologic:accumulator(), any()) -> {reduce, hydrologic:accumulator()}.
from('__end__', Acc, _) ->
  {reduce, Acc};
from(Data, [], Data) ->
  {reduce, [Data]};
from(_, [], _) ->
  {reduce, []};
from(Data, [From|_] = Acc, From) ->
  {reduce, Acc ++ [Data]}.

-spec to(any(), any()) -> {reduce, hydrologic:accumulator()}.
to(Data, _) ->
  {reduce, [Data]}.
-spec to(any(), hydrologic:accumulator(), any()) -> {reduce, hydrologic:accumulator()}.
to('__end__', Acc, _) ->
  {reduce, lists:reverse(Acc)};
to(Data, Acc, To) ->
  case erlang:hd(Acc) of
    To ->
      {reduce, Acc};
    _ ->
      {reduce, [Data|Acc]}
  end.

-spec unique(any()) -> {reduce, hydrologic:accumulator()}.
unique(Data) ->
  {reduce, [Data]}.
-spec unique(any(), hydrologic:accumulator()) -> {reduce, hydrologic:accumulator()}.
unique('__end__', Acc) ->
  {reduce, lists:reverse(Acc)};
unique(Data, Acc) ->
  case lists:member(Data, Acc) of
    true ->
      {reduce, Acc};
    false ->
      {reduce, [Data|Acc]}
  end.

-spec head(any(), non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
head(_, N) when N == 0 ->
  {reduce, []};
head(Data, _) ->
  {reduce, [Data]}.
-spec head(any(), hydrologic:accumulator(), non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
head('__end__', Acc, _) ->
  {reduce, lists:reverse(Acc)};
head(_, _, 0) ->
  {reduce, []};
head(Data, Acc, N) when length(Acc) < N ->
  {reduce, [Data|Acc]};
head(_, Acc, _) ->
  {reduce, Acc}.

-spec tail(any(), non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
tail(_, N) when N == 0 ->
  {reduce, []};
tail(Data, _) ->
  {reduce, [Data]}.
-spec tail(any(), hydrologic:accumulator(), non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
tail('__end__', Acc, _) ->
  {reduce, Acc};
tail(_, _, 0) ->
  {reduce, []};
tail(Data, Acc, N) when length(Acc) < N ->
  {reduce, Acc ++ [Data]};
tail(Data, [_|Acc], _) ->
  {reduce, Acc ++ [Data]}.

-spec drop(any(), head|tail, non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
drop(Data, _, _) ->
  {reduce, [Data]}.
-spec drop(any(), hydrologic:accumulator(), head|tail, non_neg_integer()) -> {reduce, hydrologic:accumulator()}.
drop('__end__', Acc, head, N) ->
  {reduce, lists:nthtail(N, lists:reverse(Acc))};
drop('__end__', Acc, tail, N) ->
  {reduce, lists:reverse(lists:nthtail(N, Acc))};
drop(Data, Acc, _, _) ->
  {reduce, [Data|Acc]}.

-spec flatten(any()) -> {reduce, hydrologic:accumulator()}.
flatten(Data) ->
  {reduce, [Data]}.
-spec flatten(any(), hydrologic:accumulator()) -> {reduce, hydrologic:accumulator()}.
flatten('__end__', Acc) ->
  {reduce, do_flatten(lists:reverse(Acc))};
flatten(Data, Acc) ->
  {reduce, [Data|Acc]}.

do_flatten([]) ->
  [];
do_flatten([X|Rest]) when is_list(X) ->
  do_flatten(X) ++ do_flatten(Rest);
do_flatten([X|Rest]) ->
  [X|do_flatten(Rest)].

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

-spec count(any(), chars | words | lines) -> {reduce, hydrologic:accumulator()}.
count(Data, chars) ->
  {reduce, erlang:length(bucs:to_string(Data))};
count(Data, lines) ->
  {reduce, erlang:length(string:tokens(bucs:to_string(Data), "\n"))};
count(Data, words) ->
  {reduce, erlang:length(string:tokens(bucs:to_string(Data), "\n\r\t "))}.
-spec count(any(), hydrologic:accumulator(), chars | words | lines) -> {reduce, hydrologic:accumulator()}.
count('__end__', Acc, _) ->
  {reduce, Acc};
count(Data, Acc, chars) ->
  {reduce, Acc + erlang:length(bucs:to_string(Data))};
count(Data, Acc, lines) ->
  {reduce, Acc + erlang:length(string:tokens(bucs:to_string(Data), "\n"))};
count(Data, Acc, words) ->
  {reduce, Acc + erlang:length(string:tokens(bucs:to_string(Data), "\n\r\t "))}.

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
sum('__end__', Acc) ->
  {reduce, Acc};
sum(Data, Acc) ->
  {reduce, Acc + Data}.

