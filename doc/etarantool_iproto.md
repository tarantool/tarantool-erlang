

# Module etarantool_iproto #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Tarantool/Box IPROTO encoding/decoding module.
Copyright (c) 2012 Roman Tsisyk

__Authors:__ Roman Tsisyk ([`roman@tsisyk.com`](mailto:roman@tsisyk.com)) (_web site:_ [`http://roman.tsisyk.com/`](http://roman.tsisyk.com/)).

__References__* [
Tarantool/Box IPROTO protocol reference](https://github.com/mailru/tarantool/blob/master/doc/box-protocol.txt)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_response-1">decode_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_response_body-2">decode_response_body/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_responses-3">decode_responses/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_tuple-1">decode_tuple/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_tuples-1">decode_tuples/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_tuples-2">decode_tuples/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode_field-1">encode_field/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_field-2">encode_field/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request-3">encode_request/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_call-3">encode_request_body_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_delete-3">encode_request_body_delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_insert-3">encode_request_body_insert/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_ping-0">encode_request_body_ping/0</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_replace-3">encode_request_body_replace/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_body_select-4">encode_request_body_select/4</a></td><td></td></tr><tr><td valign="top"><a href="#encode_request_header-3">encode_request_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_tuple-1">encode_tuple/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_tuples-1">encode_tuples/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode_response-1"></a>

### decode_response/1 ###

`decode_response(Binary) -> any()`


<a name="decode_response_body-2"></a>

### decode_response_body/2 ###

`decode_response_body(X1, Type) -> any()`


<a name="decode_responses-3"></a>

### decode_responses/3 ###

`decode_responses(Packet, ReduceFun, Args) -> any()`


<a name="decode_tuple-1"></a>

### decode_tuple/1 ###

`decode_tuple(X1) -> any()`


<a name="decode_tuples-1"></a>

### decode_tuples/1 ###

`decode_tuples(X1) -> any()`


<a name="decode_tuples-2"></a>

### decode_tuples/2 ###

`decode_tuples(Binary, Count) -> any()`


<a name="encode_field-1"></a>

### encode_field/1 ###

`encode_field(Field) -> any()`


<a name="encode_field-2"></a>

### encode_field/2 ###

`encode_field(Field, Tail) -> any()`


<a name="encode_request-3"></a>

### encode_request/3 ###

`encode_request(Type, RequestId, Body) -> any()`


<a name="encode_request_body_call-3"></a>

### encode_request_body_call/3 ###

`encode_request_body_call(ProcName, Args, Opts) -> any()`


<a name="encode_request_body_delete-3"></a>

### encode_request_body_delete/3 ###

`encode_request_body_delete(SpaceId, X2, Opts) -> any()`


<a name="encode_request_body_insert-3"></a>

### encode_request_body_insert/3 ###

`encode_request_body_insert(SpaceId, X2, Opts) -> any()`


<a name="encode_request_body_ping-0"></a>

### encode_request_body_ping/0 ###

`encode_request_body_ping() -> any()`


<a name="encode_request_body_replace-3"></a>

### encode_request_body_replace/3 ###

`encode_request_body_replace(SpaceId, X2, Opts) -> any()`


<a name="encode_request_body_select-4"></a>

### encode_request_body_select/4 ###

`encode_request_body_select(SpaceId, IndexId, Tuples, Opts) -> any()`


<a name="encode_request_header-3"></a>

### encode_request_header/3 ###

`encode_request_header(Type, BodyLength, RequestId) -> any()`


<a name="encode_tuple-1"></a>

### encode_tuple/1 ###

`encode_tuple(Fields) -> any()`


<a name="encode_tuples-1"></a>

### encode_tuples/1 ###

`encode_tuples(Tuples) -> any()`


