

#Module etarantool_varint#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Tarantool/Box varuint32 (LEB128) encoding/decoding module.

Copyright (c) 2012 Roman Tsisyk

__Authors:__ Roman Tsisyk ([`roman@tsisyk.com`](mailto:roman@tsisyk.com)) (_web site:_ [`http://roman.tsisyk.com/`](http://roman.tsisyk.com/)).

__References__* [
Wikipedia Article](http://en.wikipedia.org/wiki/LEB128)
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_varuint32-1">decode_varuint32/1</a></td><td>Decodes <code>Value</code> from the <code>Binary</code> chunk.</td></tr><tr><td valign="top"><a href="#encode_varuint32-1">encode_varuint32/1</a></td><td>Encodes <code>Value</code> into varuint32 format.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="decode_varuint32-1"></a>

###decode_varuint32/1##


<pre>decode_varuint32(Binary :: binary()) -&gt;{Value :: pos_integer(), Tail :: binary()}</pre>

Decodes `Value` from the `Binary` chunk.
`Tail` is the remain part of the source `Binary`.<a name="encode_varuint32-1"></a>

###encode_varuint32/1##


<pre>encode_varuint32(Value :: pos_integer()) -&gt; binary()</pre>

Encodes `Value` into varuint32 format.