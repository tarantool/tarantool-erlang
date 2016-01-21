

# Erlang connector to 1.5 #

Copyright (c) 2012-2013 Roman Tsisyk

__Authors:__ Roman Tsisyk ([`roman@tsisyk.com`](mailto:roman@tsisyk.com)).

__References__
* [Tarantool homepage](http://tarantool.org/)
* [Tarantool IPROTO protocol reference](https://github.com/tarantool/tarantool/blob/master/doc/sphinx/dev_guide/box-protocol.rst)




### <a name="Overview">Overview</a> ###


ETarantool is an Erlang client for Tarantool NoSQL database.

Tarantool is an efficient in-memory data store.
This library uses Tarantool's binary request/response protocol, called IPROTO.
IPROTO features a complete access to Tarantool functionality, including:
* request multiplexing, e.g. ability to asynchronously issue multiple
    requests via the same connection
* response format that supports zero-copy writes



### <a name="Status">Status</a> ###

Early alpha. INSERT, SELECT, REPLACE and CALL is fully supported.
UPDATE is not implemented. Request multiplexing is supported.


### <a name="Installation">Installation</a> ###

Please use [rebar](https://github.com/basho/rebar).
The following lines are needed in your `rebar.config` in order to get this work:

```
{lib_dirs,["deps"]}.
{deps, [
    {'etarantool', ".*", {git, "git://github.com/rtsisyk/etarantool.git",
        {branch, "master"}}},
]}.
```


### <a name="Examples">Examples</a> ###


```
%% Connect
> {ok, Conn} = etarantool:connect("localhost").
{ok,<0.55.0>}

%% Insert
> {ok, Tuples} = etarantool:insert(Conn, 0, [{1, 2, "text"}], [return_tuple]).
{ok,[[<<1,0,0,0>>,<<2,0,0,0>>,<<"text">>]]}

%% Select
> {ok, Tuples} = etarantool:select(Conn, 0, 0, [{1}]).
{ok,[[<<1,0,0,0>>,<<2,0,0,0>>,<<"text">>]]}

%% Call
> {ok, Tuples} = etarantool:call(Conn, <<"box.select">>, [0, 0, 1]).
{ok,[[<<1,0,0,0>>,<<2,0,0,0>>,<<"text">>]]}

%% Delete
> {ok, Tuples} = etarantool:delete(Conn, 0, [{1}], [return_tuple]).
{ok,[[<<1,0,0,0>>,<<2,0,0,0>>,<<"text">>]]}

%% Close
> ok = etarantool:close(Conn).
ok
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/rtsisyk/etarantool/blob/master/doc/etarantool.md" class="module">etarantool</a></td></tr>
<tr><td><a href="http://github.com/rtsisyk/etarantool/blob/master/doc/etarantool_iproto.md" class="module">etarantool_iproto</a></td></tr>
<tr><td><a href="http://github.com/rtsisyk/etarantool/blob/master/doc/etarantool_varint.md" class="module">etarantool_varint</a></td></tr></table>

