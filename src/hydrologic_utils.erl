% @hidden
-module(hydrologic_utils).

-export([log/4]).

log(Type, Module, Message, _Options) ->
  % TODO
  io:format("== [~p] ~p: ~p~n", [Type, Module, Message]).
