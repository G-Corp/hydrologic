-define(IS_RECEIVER(X), (is_pid(X) orelse
                         (is_atom(X) andalso
                          % Not and strlib function
                          X =/= odd andalso
                          X =/= even andalso
                          % Not a core function
                          X =/= duplicate andalso
                          X =/= fanin andalso
                          X =/= merge))).
-define(IS_WORKER(X), (is_function(X) orelse
                       (is_tuple(X) andalso (size(X) == 3 orelse size(X) == 2)) orelse
                       is_atom(X))).

-define(FLOW(Data), (erlang:is_list(Data) andalso
                     not bucs:is_string(Data)) orelse
                    bucs:is_list_of_lists(Data)).

