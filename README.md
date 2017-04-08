

# Hydrologic - A pipeline system #

Copyright (c) 2017 Grégoire Lejeune, 2017 Botsunit

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).

<a name="streams"></a>

### Streams ###

TODO


### CORE stages ###


#### duplicate ####

`duplicate` create a copy of the records from the input stream, send the
original records to the next output stream, and the copy to a second stream.

__Syntax:__<br />
_hydro:_`duplicate <Label>:`<br />
_erlang:_`{duplicate Label :: atom()}`

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
_hydro:_`:<Label> merge <Function>`<br />
_erlang:_`{Label :: atom(), {merge, Function :: function(2)}}`

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
_hydro:_`:<Label> fanin [0|1]`<br />
_erlang:_`{Label :: atom(), fanin} | {Label :: atom(), {fanin, 0 | 1}}`

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

`console` (map)

`{console, [Format :: string()]}` (map)

`{match, [Regex :: string()]}` (filter)

`{pad, [Direction :: left | right, Size :: integer(), Char :: integer()]} | {pad, [Size :: integer(), Char :: integer()]}` (map)

`{chop, [Size :: integer()]}` (map)

`{between, [Min :: string(), Max :: string()]}` (filter)

`odd` (filter)

`even` (filter)


### Create a worker ###

```

-spec worker(data(), ...) -> {map, data()}
                        | data()
                        | {filter, true | false}
                        | {return, data()}
                        | {error, term()}.
-spec worker(data(), accumulator(), ...) -> {reduce, accumulator()}.

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
* STDLIB: split Pattern - (map, elem->list)
* STDLIB: join Separator - (map, list->elem)
* STDLIB: strip left|right|both - (map, elem)
* STDLIB: replace Regex Repl - (map, elem)


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/G-Corp/hydrologic/blob/master/doc/hydrologic.md" class="module">hydrologic</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/hydrologic/blob/master/doc/hydrologic_compiler.md" class="module">hydrologic_compiler</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/hydrologic/blob/master/doc/hydrologic_stdlib.md" class="module">hydrologic_stdlib</a></td></tr></table>

