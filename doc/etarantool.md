

# Module etarantool #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


ETarantool is an Erlang client for Tarantool/Box NoSQL database.
Copyright (c) 2012 Roman Tsisyk

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Roman Tsisyk ([`roman@tsisyk.com`](mailto:roman@tsisyk.com)) (_web site:_ [`http://roman.tsisyk.com/`](http://roman.tsisyk.com/)).

<a name="types"></a>

## Data Types ##




### <a name="type-conn">conn()</a> ###



<pre><code>
conn() = pid()
</code></pre>



  Tarantool Connection



### <a name="type-field">field()</a> ###



<pre><code>
field() = binary() | pos_integer() | list()
</code></pre>



  Acceptable field types for all methods. Please note, that fields in
the tuples returned from the server always have `binary()` type.



### <a name="type-field_id">field_id()</a> ###



<pre><code>
field_id() = integer()
</code></pre>



  Tarantool's Field Identifier.



### <a name="type-index_id">index_id()</a> ###



<pre><code>
index_id() = integer()
</code></pre>



  Tarantool'sIndex Identifier in the space.



### <a name="type-result_count">result_count()</a> ###



<pre><code>
result_count() = {ok, integer()}
</code></pre>



  Query result (number of affected tuples)



### <a name="type-result_error">result_error()</a> ###



<pre><code>
result_error() = {error, ErrorCode :: <a href="#type-return_code">return_code()</a>, Reason :: any()}
</code></pre>



  Query result (return code and error message)



### <a name="type-result_tuples">result_tuples()</a> ###



<pre><code>
result_tuples() = {ok, [[<a href="#type-field">field()</a>]]}
</code></pre>



  Query result (tuples)



### <a name="type-return_code">return_code()</a> ###



<pre><code>
return_code() = atom() | integer()
</code></pre>



  Predefined return code



### <a name="type-space_id">space_id()</a> ###



<pre><code>
space_id() = integer()
</code></pre>



  Tarantool's Space Identifier
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-3">call/3</a></td><td>Equivalent to <a href="#call-4"><tt>call(Conn, ProcName, Args, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#call-4">call/4</a></td><td>Execute stored procedure on the server.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Closes the connection <code>Conn</code> and stops <code>gen_server</code>.</td></tr><tr><td valign="top"><a href="#connect-1">connect/1</a></td><td>Equivalent to <a href="#connect-2"><tt>connect(Host, 33013)</tt></a>.</td></tr><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>Equivalent to <a href="#connect-3"><tt>connect(Host, Port, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td>Connects to a server <code>Host</code>:<code>Port</code> using Tarantool's binary protocol
(IPROTO).</td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td>Equivalent to <a href="#delete-4"><tt>delete(Conn, SpaceId, Tuples, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#delete-4">delete/4</a></td><td>Deletes <code>Tuples</code> in the space <code>SpaceId</code> using <code>Conn</code>.</td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>Equivalent to <a href="#insert-4"><tt>insert(Conn, SpaceId, Tuples, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#insert-4">insert/4</a></td><td>Inserts <code>Tuples</code> to the space <code>SpaceId</code> using <code>Conn</code>.</td></tr><tr><td valign="top"><a href="#ping-1">ping/1</a></td><td>Sends PING request using <code>Conn</code>.</td></tr><tr><td valign="top"><a href="#replace-3">replace/3</a></td><td>Equivalent to <a href="#replace-4"><tt>replace(Conn, SpaceId, Tuples, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#replace-4">replace/4</a></td><td>Replaces <code>Tuples</code> in the space <code>SpaceId</code> using <code>Conn</code>.</td></tr><tr><td valign="top"><a href="#select-4">select/4</a></td><td>Equivalent to <a href="#select-5"><tt>select(Conn, SpaceId, IndexId, Tuples, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#select-5">select/5</a></td><td>Selects tuples from the space <code>SpaceId</code> by <code>IndexId</code> using <code>Conn</code>
where space keys match <code>Tuples</code> keys.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-3"></a>

### call/3 ###


<pre><code>
call(Conn :: <a href="#type-conn">conn()</a>,ProcName :: <a href="#type-field">field()</a>,Args :: [<a href="#type-field">field()</a>] | tuple()) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>


Equivalent to [`call(Conn, ProcName, Args, [])`](#call-4).
<a name="call-4"></a>

### call/4 ###


<pre><code>
call(Conn :: <a href="#type-conn">conn()</a>,ProcName :: <a href="#type-field">field()</a>,Args :: [<a href="#type-field">field()</a>] | tuple(),Opts :: [<a href="proplists.md#type-property">proplists:property</a>(any())]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>



Execute stored procedure on the server.


ProcName(*Args) where `*` means `unpack arguments` will be executed
on the server.
<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Conn :: <a href="#type-conn">conn()</a>) -&gt; ok
</code></pre>


Closes the connection `Conn` and stops `gen_server`.
<a name="connect-1"></a>

### connect/1 ###

`connect(Address) -> any()`

Equivalent to [`connect(Host, 33013)`](#connect-2).
<a name="connect-2"></a>

### connect/2 ###

`connect(Address, Port) -> any()`

Equivalent to [`connect(Host, Port, [])`](#connect-3).
<a name="connect-3"></a>

### connect/3 ###


<pre><code>
connect(Address :: <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>,Port :: <a href="inet.md#type-port_number">inet:port_number()</a>,Opts :: <a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt;{ok, Conn :: <a href="#type-conn">conn()</a>} |{error, ErrorCode :: atom(), Reason :: list()}
</code></pre>



Connects to a server `Host`:`Port` using Tarantool's binary protocol
(IPROTO). The `Address` argument can be either a hostname, or an IP address.


`Opts` is `proplist` with the following options:

* __mode__ - configures how to process results from a server

1. __blocked__ (default) - perform requests in blocking mode.
Client waits for the result and then returns it to the user.

1. __async__ -  perform request in asynchronous mode. Client
returns `{ok, RequestId}` immediately and sends message
`{etarantool, Conn, RequestId, Result}` with the same `RequestId`
to the callee when the response is got from a server.

1. __discard__ - discard all results. Client returns
`{ok, RequestId}` immediately and ignores all responces from a
server (including error messages).





<a name="delete-3"></a>

### delete/3 ###


<pre><code>
delete(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>


Equivalent to [`delete(Conn, SpaceId, Tuples, [])`](#delete-4).
<a name="delete-4"></a>

### delete/4 ###


<pre><code>
delete(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()],Opts :: [<a href="proplists.md#type-property">proplists:property</a>(return_tuple, true)]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>



Deletes `Tuples` in the space `SpaceId` using `Conn`.


`Opts` is `proplist` with following options:

* __return_tuple__ - return removed tuples (`result_tuples()`)
instead of tuples count (`result_count()`)



Please note, that due to limitations of the protocol only one tuple can be
placed in `Tuples` list. This limitation may be removed in the future.

<a name="insert-3"></a>

### insert/3 ###


<pre><code>
insert(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>


Equivalent to [`insert(Conn, SpaceId, Tuples, [])`](#insert-4).
<a name="insert-4"></a>

### insert/4 ###


<pre><code>
insert(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()],Opts :: [<a href="proplists.md#type-property">proplists:property</a>(return_tuple, true)]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>



Inserts `Tuples` to the space `SpaceId` using `Conn`.



`insert` requires that no tuple with the same pkey exists in the space.


`Opts` is `proplist` with following options:

* __return_tuple__ - return inserted tuples (`result_tuples()`)
instead of tuples count (`result_count()`)



Please note, that due to limitations of the protocol only one tuple can be
placed in `Tuples` list. This limitation may be removed in the future.

<a name="ping-1"></a>

### ping/1 ###


<pre><code>
ping(Conn :: <a href="#type-conn">conn()</a>) -&gt; ok
</code></pre>


Sends PING request using `Conn`.
<a name="replace-3"></a>

### replace/3 ###


<pre><code>
replace(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>


Equivalent to [`replace(Conn, SpaceId, Tuples, [])`](#replace-4).
<a name="replace-4"></a>

### replace/4 ###


<pre><code>
replace(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()],Opts :: [<a href="proplists.md#type-property">proplists:property</a>(return_tuple, true)]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_count">result_count()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>



Replaces `Tuples` in the space `SpaceId` using `Conn`.



`replace` requires that a tuple with the same pkey is present in the space.


`Opts` is `proplist` with following options:

* __return_tuple__ - return replaced tuples (`result_tuples()`)
instead of tuples count (`result_count()`)



Please note, that due to limitations of the protocol only one tuple can be
placed in `Tuples` list. This limitation may be removed in the future.

<a name="select-4"></a>

### select/4 ###

`select(Conn, SpaceId, IndexId, Tuples) -> any()`

Equivalent to [`select(Conn, SpaceId, IndexId, Tuples, [])`](#select-5).
<a name="select-5"></a>

### select/5 ###


<pre><code>
select(Conn :: <a href="#type-conn">conn()</a>,SpaceId :: <a href="#type-space_id">space_id()</a>,IndexId :: <a href="#type-index_id">index_id()</a>,Tuples :: [[<a href="#type-field">field()</a>] | tuple()],Opts ::[<a href="proplists.md#type-property">proplists:property</a>(offset, integer()) |<a href="proplists.md#type-property">proplists:property</a>(limit, integer())]) -&gt;<a href="#type-result_tuples">result_tuples()</a> | <a href="#type-result_error">result_error()</a>
</code></pre>



Selects tuples from the space `SpaceId` by `IndexId` using `Conn`
where space keys match `Tuples` keys.



You do not need entire tuple to select (of course), you just need indexed
keys. These keys must be in same order as defined in the space configuration.


`Opts` is `proplist` with following options:

* __offset__ - skip a specified number of tuples from result.
Default is 0.

* __limit__ - limit number of resulting tuples.
Default is -1.



