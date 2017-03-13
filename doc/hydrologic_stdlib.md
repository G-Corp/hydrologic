

# Module hydrologic_stdlib #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#between-3">between/3</a></td><td></td></tr><tr><td valign="top"><a href="#chop-2">chop/2</a></td><td></td></tr><tr><td valign="top"><a href="#console-1">console/1</a></td><td></td></tr><tr><td valign="top"><a href="#console-2">console/2</a></td><td></td></tr><tr><td valign="top"><a href="#count-2">count/2</a></td><td></td></tr><tr><td valign="top"><a href="#count-3">count/3</a></td><td></td></tr><tr><td valign="top"><a href="#even-1">even/1</a></td><td></td></tr><tr><td valign="top"><a href="#from-2">from/2</a></td><td></td></tr><tr><td valign="top"><a href="#from-3">from/3</a></td><td></td></tr><tr><td valign="top"><a href="#head-2">head/2</a></td><td></td></tr><tr><td valign="top"><a href="#head-3">head/3</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td></td></tr><tr><td valign="top"><a href="#odd-1">odd/1</a></td><td></td></tr><tr><td valign="top"><a href="#pad-3">pad/3</a></td><td></td></tr><tr><td valign="top"><a href="#pad-4">pad/4</a></td><td></td></tr><tr><td valign="top"><a href="#return-1">return/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td></td></tr><tr><td valign="top"><a href="#sum-1">sum/1</a></td><td></td></tr><tr><td valign="top"><a href="#sum-2">sum/2</a></td><td></td></tr><tr><td valign="top"><a href="#tail-2">tail/2</a></td><td></td></tr><tr><td valign="top"><a href="#tail-3">tail/3</a></td><td></td></tr><tr><td valign="top"><a href="#to-2">to/2</a></td><td></td></tr><tr><td valign="top"><a href="#to-3">to/3</a></td><td></td></tr><tr><td valign="top"><a href="#unique-1">unique/1</a></td><td></td></tr><tr><td valign="top"><a href="#unique-2">unique/2</a></td><td></td></tr></table>


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

<a name="count-2"></a>

### count/2 ###

<pre><code>
count(Data::any(), X2::chars | words | lines) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="count-3"></a>

### count/3 ###

<pre><code>
count(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>, X3::chars | words | lines) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="even-1"></a>

### even/1 ###

<pre><code>
even(Data::integer()) -&gt; {filter, boolean()}
</code></pre>
<br />

<a name="from-2"></a>

### from/2 ###

<pre><code>
from(Data::any(), X2::any()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="from-3"></a>

### from/3 ###

<pre><code>
from(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>, From::any()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="head-2"></a>

### head/2 ###

<pre><code>
head(Data::any(), N::non_neg_integer()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="head-3"></a>

### head/3 ###

<pre><code>
head(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>, N::non_neg_integer()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
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

<a name="sort-1"></a>

### sort/1 ###

<pre><code>
sort(Data::any()) -&gt; {reduce, list()}
</code></pre>
<br />

<a name="sort-2"></a>

### sort/2 ###

<pre><code>
sort(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="sum-1"></a>

### sum/1 ###

<pre><code>
sum(Data::integer()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="sum-2"></a>

### sum/2 ###

<pre><code>
sum(Data::integer(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="tail-2"></a>

### tail/2 ###

<pre><code>
tail(Data::any(), N::non_neg_integer()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="tail-3"></a>

### tail/3 ###

<pre><code>
tail(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>, N::non_neg_integer()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="to-2"></a>

### to/2 ###

<pre><code>
to(Data::any(), X2::any()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="to-3"></a>

### to/3 ###

<pre><code>
to(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>, To::any()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="unique-1"></a>

### unique/1 ###

<pre><code>
unique(Data::any()) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

<a name="unique-2"></a>

### unique/2 ###

<pre><code>
unique(Data::any(), Acc::<a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>) -&gt; {reduce, <a href="hydrologic.md#type-accumulator">hydrologic:accumulator()</a>}
</code></pre>
<br />

