

# Module hydrologic_stdlib #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#between-3">between/3</a></td><td></td></tr><tr><td valign="top"><a href="#chop-2">chop/2</a></td><td></td></tr><tr><td valign="top"><a href="#console-1">console/1</a></td><td></td></tr><tr><td valign="top"><a href="#console-2">console/2</a></td><td></td></tr><tr><td valign="top"><a href="#even-1">even/1</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#odd-1">odd/1</a></td><td></td></tr><tr><td valign="top"><a href="#pad-3">pad/3</a></td><td></td></tr><tr><td valign="top"><a href="#pad-4">pad/4</a></td><td></td></tr><tr><td valign="top"><a href="#return-1">return/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="between-3"></a>

### between/3 ###

<pre><code>
between(Data::any(), Min::any(), Max::any()) -&gt; {filter, boolean()}
</code></pre>
<br />

<a name="chop-2"></a>

### chop/2 ###

<pre><code>
chop(Data::any(), Size::non_neg_integer()) -&gt; {map, any()}
</code></pre>
<br />

<a name="console-1"></a>

### console/1 ###

<pre><code>
console(Data::any()) -&gt; {map, any()}
</code></pre>
<br />

<a name="console-2"></a>

### console/2 ###

<pre><code>
console(Data::any(), Format::string()) -&gt; {map, any()}
</code></pre>
<br />

<a name="even-1"></a>

### even/1 ###

<pre><code>
even(Data::integer()) -&gt; {filter, boolean()}
</code></pre>
<br />

<a name="match-2"></a>

### match/2 ###

<pre><code>
match(Data::any(), Regex::string() | binary()) -&gt; {filter, boolean()}
</code></pre>
<br />

<a name="odd-1"></a>

### odd/1 ###

<pre><code>
odd(Data::integer()) -&gt; {filter, boolean()}
</code></pre>
<br />

<a name="pad-3"></a>

### pad/3 ###

<pre><code>
pad(Data::any(), Size::non_neg_integer(), Char::integer()) -&gt; {map, any()}
</code></pre>
<br />

<a name="pad-4"></a>

### pad/4 ###

<pre><code>
pad(Data::any(), X2::right | left, Size::non_neg_integer(), Char::integer()) -&gt; {map, any()}
</code></pre>
<br />

<a name="return-1"></a>

### return/1 ###

<pre><code>
return(Data::any()) -&gt; {return, any()}
</code></pre>
<br />

