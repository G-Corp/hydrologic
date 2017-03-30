-module(hydrologic_stdlib).

-export([
         return/1
         , console/1
         , console/2
         , sort/1
         , from/2
         , to/2
         , unique/1
         , head/2
         , tail/2
         , drop/3
         , flatten/1
         , duplicate/2
        ]).

-export([
         match/2
         , pad/3
         , pad/4
         , chop/2
         , between/3
         , count/2
        ]).

-export([
         odd/1
         , even/1
         , sum/1
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

-spec sort(hydrologic:reduct()) -> {reduce, list()}.
sort({'__end__', Acc}) ->
  {reduce, lists:sort(Acc)};
sort({Data, Acc}) ->
  {reduce, [Data|Acc]};
sort(Data) ->
  {reduce, [Data]}.

-spec from(hydrologic:reduct(), any()) -> {reduce, list()}.
from({'__end__', Acc}, _) ->
  {reduce, Acc};
from({Data, []}, Data) ->
  {reduce, [Data]};
from({_, []}, _) ->
  {reduce, []};
from({Data, [From|_] = Acc}, From) ->
  {reduce, Acc ++ [Data]};
from(Data, Data) ->
  {reduce, [Data]};
from(_, _) ->
  {reduce, []}.

-spec to(hydrologic:reduct(), any()) -> {reduce, list()}.
to({'__end__', Acc}, _) ->
  {reduce, lists:reverse(Acc)};
to({Data, Acc}, To) ->
  case erlang:hd(Acc) of
    To ->
      {reduce, Acc};
    _ ->
      {reduce, [Data|Acc]}
  end;
to(Data, _) ->
  {reduce, [Data]}.

-spec unique(hydrologic:reduct()) -> {reduce, list()}.
unique({'__end__', Acc}) ->
  {reduce, lists:reverse(Acc)};
unique({Data, Acc}) ->
  case lists:member(Data, Acc) of
    true ->
      {reduce, Acc};
    false ->
      {reduce, [Data|Acc]}
  end;
unique(Data) ->
  {reduce, [Data]}.

-spec head(hydrologic:reduct(), non_neg_integer()) -> {reduce, list()}.
head({'__end__', Acc}, _) ->
  {reduce, lists:reverse(Acc)};
head({_, _}, 0) ->
  {reduce, []};
head({Data, Acc}, N) when length(Acc) < N ->
  {reduce, [Data|Acc]};
head({_, Acc}, _) ->
  {reduce, Acc};
head(_, N) when N == 0 ->
  {reduce, []};
head(Data, _) ->
  {reduce, [Data]}.

-spec tail(hydrologic:reduct(), non_neg_integer()) -> {reduce, list()}.
tail({'__end__', Acc}, _) ->
  {reduce, Acc};
tail({_, _}, 0) ->
  {reduce, []};
tail({Data, Acc}, N) when length(Acc) < N ->
  {reduce, Acc ++ [Data]};
tail({Data, [_|Acc]}, _) ->
  {reduce, Acc ++ [Data]};
tail(_, N) when N == 0 ->
  {reduce, []};
tail(Data, _) ->
  {reduce, [Data]}.

-spec drop(hydrologic:reduct(), head|tail, non_neg_integer()) -> {reduce, list()}.
drop({'__end__', Acc}, head, N) ->
  {reduce, lists:nthtail(N, lists:reverse(Acc))};
drop({'__end__', Acc}, tail, N) ->
  {reduce, lists:reverse(lists:nthtail(N, Acc))};
drop({Data, Acc}, _, _) ->
  {reduce, [Data|Acc]};
drop(Data, _, _) ->
  {reduce, [Data]}.

-spec flatten(hydrologic:reduct()) -> {reduce, list()}.
flatten({'__end__', Acc}) ->
  {reduce, do_flatten(lists:reverse(Acc))};
flatten({Data, Acc}) ->
  {reduce, [Data|Acc]};
flatten(Data) ->
  {reduce, [Data]}.

do_flatten([]) ->
  [];
do_flatten([X|Rest]) when is_list(X) ->
  do_flatten(X) ++ do_flatten(Rest);
do_flatten([X|Rest]) ->
  [X|do_flatten(Rest)].

-spec duplicate(hydrologic:reduct(), non_neg_integer()) -> {reduce, list()}.
duplicate({'__end__', Acc}, _) ->
  {reduce, Acc};
duplicate({Data, Acc}, N) ->
  {reduce, Acc ++ lists:duplicate(N, Data)};
duplicate(Data, N) ->
  {reduce, lists:duplicate(N, Data)}.

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

-spec count(hydrologic:reduct(), chars | words | lines) -> {reduce, non_neg_integer()}.
count({'__end__', Acc}, _) ->
  {reduce, Acc};
count({Data, Acc}, chars) ->
  {reduce, Acc + erlang:length(bucs:to_string(Data))};
count({Data, Acc}, lines) ->
  {reduce, Acc + erlang:length(string:tokens(bucs:to_string(Data), "\n"))};
count({Data, Acc}, words) ->
  {reduce, Acc + erlang:length(string:tokens(bucs:to_string(Data), "\n\r\t "))};
count(Data, chars) ->
  {reduce, erlang:length(bucs:to_string(Data))};
count(Data, lines) ->
  {reduce, erlang:length(string:tokens(bucs:to_string(Data), "\n"))};
count(Data, words) ->
  {reduce, erlang:length(string:tokens(bucs:to_string(Data), "\n\r\t "))}.

% Integer

-spec even(integer()) -> {filter, boolean()}.
even(Data) ->
  {filter, Data rem 2 == 0}.

-spec odd(integer()) -> {filter, boolean()}.
odd(Data) ->
  {filter, Data rem 2 /= 0}.

-spec sum(hydrologic:reduct()) -> {reduce, non_neg_integer()}.
sum({'__end__', Acc}) ->
  {reduce, Acc};
sum({Data, Acc}) ->
  {reduce, Acc + Data};
sum(Data) ->
  {reduce, Data}.

