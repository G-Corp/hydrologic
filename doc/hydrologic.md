

# Module hydrologic #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###


<pre><code>
data() = any()
</code></pre>




### <a name="type-operation">operation()</a> ###


<pre><code>
operation() = term()
</code></pre>




### <a name="type-pipe">pipe()</a> ###


<pre><code>
pipe() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>
Create a new pipe.</td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td>.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Destroy the given pipe.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Name::atom(), Operations::[<a href="#type-operation">operation()</a>]) -&gt; <a href="#type-pipe">pipe()</a>
</code></pre>
<br />

Create a new pipe.

<a name="run-2"></a>

### run/2 ###

<pre><code>
run(Pipe::<a href="#type-pipe">pipe()</a>, Data::<a href="#type-data">data()</a>) -&gt; {ok, <a href="#type-data">data()</a>} | {error, term(), <a href="#type-data">data()</a>} | {error, term()} | any()
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Pipe::<a href="#type-pipe">pipe()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Destroy the given pipe.

