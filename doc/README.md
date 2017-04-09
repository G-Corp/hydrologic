

# Hydrologic - A pipeline system #

Copyright (c) 2017 Grégoire Lejeune, 2017 Botsunit

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).


### Streams ###

TODO


### CORE stages ###


#### duplicate ####

`duplicate` create a copy of the records from the input stream, send the
original records to the next output stream, and the copy to a second stream.

__Syntax:__<br />
_hydro:_ `duplicate <Label>:`<br />
_erlang:_ `{duplicate Label :: atom()}`

__Availability:__ _record_, _list_

```

                                     +------------+
                                     |            |
                                 +-->| :L [stage] |
+---------+    +--------------+  |   |            |
|         |    |              |--+   +------------+
| [stage] |--->| duplicate L: |
|         |    |              |--+   +------------+
+---------+    +--------------+  |   |            |
                                 +-->|  [stage]   |
                                     |            |
                                     +------------+

```

Example :

hydro:

```

% sample.hydro

| fun(X) -> 2 * X end
| duplicate a:
| fun(X) -> 3 * X end
| :n merge fun(X1, X2) -> X1 + X2 end
?
| :a fun(X) -> X + X end
| b:

```
erlang :

```

hydrologic:new(
  sample,
  [
   fun(X) -> 2*X end, % 2 4 6
   {duplicate, a},
   fun(X) -> 3*X end, % 6 12 18
   {b, {merge, fun(X1, X2) ->
                   X1 + X2 % 10 20 30
               end}},
   return,
   {a, fun(X) -> % 4 8 12
           X + X
       end},
   b
  ]
 )

```


#### merge ####

`merge` send all records from the input streams to a function that will return a combined record and send it to the output stream.

__Syntax:__<br />
_hydro:_ `:<Label> merge <Function>`<br />
_erlang:_ `{Label :: atom(), {merge, Function :: function(2)}}`

__Availability:__ _record_, _list_

```

+------------+
|            |
| [stage] L: |--+
|            |  |   +-------------------+    +---------+
+------------+  +-->|                   |    |         |
                    | :L merge Function |--->| [stage] |
+------------+  +-->|                   |    |         |
|            |  |   +-------------------+    +---------+
|  [stage]   |--+
|            |
+------------+

```

Example :

hydro:

```

% sample.hydro

| fun(X) -> 2 * X end
| duplicate a:
| fun(X) -> 3 * X end
| :n merge fun(X1, X2) -> X1 + X2 end
?
| :a fun(X) -> X + X end
| b:

```
erlang :

```

hydrologic:new(
  sample,
  [
   fun(X) -> 2*X end, % 2 4 6
   {duplicate, a},
   fun(X) -> 3*X end, % 6 12 18
   {b, {merge, fun(X1, X2) ->
                   X1 + X2 % 10 20 30
               end}},
   return,
   {a, fun(X) -> % 4 8 12
           X + X
       end},
   b
  ]
 )

```


#### fanin ####

`fanin` reassemble the input streams.

__Syntax :__<br />
_hydro:_ `:<Label> fanin [0|1]`<br />
_erlang:_ `{Label :: atom(), fanin} | {Label :: atom(), {fanin, 0 | 1}}`

__Availability:__ _list_

```

+------------+
|            |
| [stage] L: |--+
|            |  |   +------------------+    +---------+
+------------+  +-->|                  |    |         |
                    | :L fanin [Order] |--->| [stage] |
+------------+  +-->|                  |    |         |
|            |  |   +------------------+    +---------+
|  [stage]   |--+
|            |
+------------+

```

Example :

hydro:

```

% sample.hydro

| odd a:
| :b fanin
?
| :a fun(X) -> {map, X * 10} end
| b:

```
erlang:

```

hydrologic:new(
  sample,
  [
   {odd, a},
   {b, fanin},
   return,
   {a, fun(X) ->
           {map, X * 10}
       end},
   b
  ]
 )

```


### STDLIB ###


#### return ####

`return` terminate the current stream.

__Syntax:__<br />
_hydro:_ `return | ?`<br />
_erlang:_ `return`

__Availability:__ _record_, _list_

```

+---------+    +--------+
|         |    |        |
| [stage] |--->| return |
|         |    |        |
+---------+    +--------+

```

Example :

hydro:

```

% sample.hydro

| even a:
? :b
| :a fun(X) -> X + 1 end
| b:

```
erlang:

```

hydrologic:new(
  sample,
  [
    {even, a},
    {b, return},
    {a, {fun(X) -> X + 1 end}}
    b
  ]
 )

```


#### console ####

`console` print the current stream on the strandard output.

__Syntax:__<br />
_hydro:_ `console [io:format()]`<br />
_erlang:_ `console | {console [io:format()]}`

For `io:format()`, see [io:format/2](http://erlang.org/doc/man/io.md#format-2).

__Availability:__ _record_, _list_

```

+---------+    +----------------+    +---------+
|         |    |                |    |         |
| [stage] |--->| console Format |--->| [stage] |
|         |    |                |    |         |
+---------+    +----------------+    +---------+

```

Example :

hydro:

```

% sample.hydro

| console "Value = ~p"
?

```
erlang:

```

hydrologic:new(
  sample,
  [
    {console, "Value = ~p"}
  ]
 )

```


#### sort ####


#### from ####


#### to ####


#### unique ####


#### head ####


#### tail ####


#### drop ####


#### flatten ####


#### repeat ####


#### match ####


#### pad ####


#### chop ####


#### between ####


#### count ####


#### join ####


#### split ####


#### odd ####


#### even ####


#### sum ####


### Create a stage ###

```

-spec fun(data(), ...) -> {map, data()}
                          | data()
                          | {filter, true | false}
                          | {return, data()}
                          | {error, term()}.

-spec fun('__empty__'
          | data()
          | {data(), accumulator()}
          | {'__end__', accumulator()}
          , ...) -> {reduce, accumulator()};

```


### Examples ###

![](https://yuml.me/diagram/scruffy;dir:LR/class/%2F%2F%20Cool%20Class%20Diagram,%20[a%7CX%20rem%202%20==%200%7Cb]-%3E[b%7CX%20+%202],%20[a%7CX%20rem%202%20==%200%7Cb]-%3E[X%20*%202],%20[X%20*%202]-%3E[b%7CX%20+%202])

```

hydrologic:new(
  test,
  [
   {a, {fun(X) ->
            {filter, X rem 2 == 0}
        end, b}},
   fun(X) ->
       {map, X * 2}
   end,
   {b, fun(X) ->
           {map, X + 2}
       end},
   return
  ]
 ),
hydrologic:run(test, 10), % => 22
hydrologic:run(test, 7). % => 9

```


### Licence ###

Hydrologic is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2017 Grégoire Lejeune<br />
Copyright (c) 2017 BotsUnit<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


### TODO ###

* [ ] (STDLIB) strip left|right|both - (map, elem)

* [ ] (STDLIB) replace Regex Repl - (map, elem)

* [ ] .hydro compiler

* [ ] rebar3 plugin for the .hydro compiler

* [ ] erlang.mk plugin for the .hydro compiler



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="hydrologic.md" class="module">hydrologic</a></td></tr>
<tr><td><a href="hydrologic_compiler.md" class="module">hydrologic_compiler</a></td></tr>
<tr><td><a href="hydrologic_stdlib.md" class="module">hydrologic_stdlib</a></td></tr></table>

