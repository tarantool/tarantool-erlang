%%
%% @copyright 2012 Roman Tsisyk
%% @author Roman Tsisyk <roman@tsisyk.com>
%%      [http://roman.tsisyk.com/]
%%
%% @doc Tarantool/Box IPROTO encoding/decoding module.
%% @end
%%
%% @reference <a href=
%% "https://github.com/mailru/tarantool/blob/master/doc/box-protocol.txt">
%% Tarantool/Box IPROTO protocol reference</a>
%%

-module(etarantool_iproto).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    encode_request/3,
    encode_request_header/3,
    encode_request_body_ping/0,
    encode_request_body_select/4,
    encode_request_body_insert/3,
    encode_request_body_replace/3,
    encode_request_body_delete/3,
    encode_request_body_call/3,

    decode_response/1,
    decode_response_header/1,
    decode_response_body/2,

    encode_field/1,
    encode_field/2,

    encode_tuple/1,
    encode_tuples/1,
    
    decode_tuple/1,
    decode_tuples/1,
    decode_tuples/2
]).

%% ------------------------------------------------------------------
%% Module Private Definitions
%% ------------------------------------------------------------------

%% IPROTO request types, see box-protocol.txt
-define(REQUEST_TYPE_INSERT, 13).
-define(REQUEST_TYPE_SELECT, 17).
-define(REQUEST_TYPE_UPDATE, 19).
-define(REQUEST_TYPE_DELETE, 21).
-define(REQUEST_TYPE_CALL, 22).
-define(REQUEST_TYPE_PING, 65280).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

encode_request(Type, RequestId, Body) ->
    BodyLength = erlang:byte_size(Body),
    Header = encode_request_header(Type, BodyLength, RequestId),
    <<Header/binary, Body/binary>>.

encode_request_header(Type, BodyLength, RequestId) ->
    <<Type:4/little-signed-integer-unit:8,
      BodyLength:4/little-signed-integer-unit:8,
      RequestId:4/little-signed-integer-unit:8>>.

encode_request_body_ping() ->
    {?REQUEST_TYPE_PING, <<>>}.

encode_request_body_select(SpaceId, IndexId, Tuples, Opts) ->
    Offset = proplists:get_value(offset, Opts, 0),
    Limit = proplists:get_value(limit, Opts, -1),
    true = is_integer(Offset),
    true = is_integer(Limit),
    TuplesBinary = encode_tuples(Tuples),
    {?REQUEST_TYPE_SELECT,
    <<SpaceId:4/integer-signed-little-unit:8,
      IndexId:4/integer-signed-little-unit:8,
      Offset:4/integer-signed-little-unit:8,
      Limit:4/integer-signed-little-unit:8,
      TuplesBinary/binary>>}.

encode_request_body_insert(SpaceId, [Tuple], Opts) ->
    Flags = case proplists:get_bool(return_tuple, Opts) of
        true  -> 3;
        false -> 2
    end,
    TupleBinary = encode_tuple(Tuple),
    {?REQUEST_TYPE_INSERT,
    <<SpaceId:4/integer-signed-little-unit:8,
      Flags:4/integer-signed-little-unit:8,
      TupleBinary/binary>>}.

encode_request_body_replace(SpaceId, [Tuple], Opts) ->
    Flags = case proplists:get_bool(return_tuple, Opts) of
        true  -> 5;
        false -> 4
    end,
    TupleBinary = encode_tuple(Tuple),
    {?REQUEST_TYPE_INSERT,
    <<SpaceId:4/integer-signed-little-unit:8,
      Flags:4/integer-signed-little-unit:8,
      TupleBinary/binary>>}.

encode_request_body_delete(SpaceId, [Tuple], Opts) ->
    Flags = case proplists:get_bool(return_tuple, Opts) of
        true  -> 1;
        false -> 0
    end,
    TupleBinary = encode_tuple(Tuple),
    {?REQUEST_TYPE_DELETE,
    <<SpaceId:4/integer-signed-little-unit:8,
      Flags:4/integer-signed-little-unit:8,
      TupleBinary/binary>>}.

encode_request_body_call(ProcName, Args, _Opts) ->
    Flags = 0,
    ProcBinary = encode_field(ProcName, encode_tuple(Args)),
    {?REQUEST_TYPE_CALL,
    <<Flags:4/integer-signed-little-unit:8,
      ProcBinary/binary>>}.

decode_response(Binary) ->
    {Type, BodyLength, RequestId, BinaryTail} = decode_response_header(Binary),
    <<Body:BodyLength/binary-unit:8, BinaryTail2/binary>> = BinaryTail,
    {Type, RequestId, Body, BinaryTail2}.

decode_response_header(Binary) ->
    <<Type:4/little-signed-integer-unit:8,
      BodyLength:4/little-signed-integer-unit:8,
      RequestId:4/little-signed-integer-unit:8,
      BinaryTail/binary>> = Binary,
    {Type, BodyLength, RequestId, BinaryTail}.

decode_response_body(?REQUEST_TYPE_PING, <<>>) ->
    {ok, []};

decode_response_body(Type, Body) ->
    <<ReturnCode:4/integer-signed-little-unit:8, BodyTail/binary>> = Body,
    decode_response_body(Type, ReturnCode, BodyTail).

decode_response_body(?REQUEST_TYPE_SELECT, 0, Body) ->
    {Tuples, <<>>} = decode_tuples(Body),
    {ok, Tuples};

decode_response_body(?REQUEST_TYPE_INSERT, 0,
        <<Count:4/integer-signed-little-unit:8>>) ->
    {ok, Count};

decode_response_body(?REQUEST_TYPE_INSERT, 0,
        <<Count:4/integer-signed-little-unit:8, Binary/binary>>) ->
    {Tuples, <<>>} = decode_tuples(Binary, Count),
    {ok, Tuples};

decode_response_body(?REQUEST_TYPE_DELETE, 0, Body) ->
    decode_response_body(?REQUEST_TYPE_INSERT, 0, Body);

decode_response_body(?REQUEST_TYPE_CALL, 0, Body) ->
    decode_response_body(?REQUEST_TYPE_SELECT, 0, Body);

decode_response_body(_Type, ReturnCode, ErrorMessage)
        when ReturnCode =/= 0 ->
    {error, decode_returncode(ReturnCode), ErrorMessage}.

encode_field(Field) ->
    encode_field(Field, <<>>).

encode_field(Field, Tail)
        when is_integer(Field), (Field >= 0), (Field < 4294967296) ->
    Field2 = <<Field:4/integer-signed-little-unit:8>>,
    encode_field(Field2, Tail);
encode_field(Field, Tail)
        when is_integer(Field), (Field >= 0), (Field < 18446744073709551616) ->
    Field2 = <<Field:8/integer-signed-little-unit:8>>,
    encode_field(Field2, Tail);
encode_field(Field, Tail)
        when is_list(Field) ->
    Field2 = erlang:list_to_binary(Field),
    encode_field(Field2, Tail);
encode_field(Field, Tail)
        when is_binary(Field) ->
    Size = erlang:byte_size(Field),
    SizeEncoded = etarantool_varint:encode_varuint32(Size),
    <<SizeEncoded/binary, Field/binary, Tail/binary>>.

encode_tuple(Fields) ->
    encode_tuple(Fields, <<>>).

encode_tuple(Fields, Tail)
        when is_tuple(Fields) ->
    encode_tuple(erlang:tuple_to_list(Fields), Tail);
encode_tuple(Fields, Tail)
    when is_list(Fields) ->
    encode_tuple(lists:reverse(Fields), 0, Tail).

encode_tuple([Field|Fields], Cardinality, Tail)  ->
    Tail2 = encode_field(Field, Tail),
    encode_tuple(Fields, Cardinality + 1, Tail2);
encode_tuple([], Cardinality, Tail) ->
    <<Cardinality:4/integer-signed-little-unit:8, Tail/binary>>.

encode_tuples(Tuples) ->
    encode_tuples(lists:reverse(Tuples), 0, <<>>).

encode_tuples([Tuple|Tuples], Count, Tail) ->
    Tail2 = encode_tuple(Tuple, Tail),
    encode_tuples(Tuples, Count + 1, Tail2);
encode_tuples([], Count, Tail) ->
    <<Count:4/integer-signed-little-unit:8, Tail/binary>>.

decode_tuple(Binary) ->
    <<Size:4/integer-signed-little-unit:8,
      Cardinality:4/integer-signed-little-unit:8,
      TupleBinary:Size/binary,
      BinaryTail/binary>> = Binary,
    {Tuple, <<>>} = decode_tuple(TupleBinary, [], Cardinality),
    {Tuple, BinaryTail}.

decode_tuple(Binary, Fields, 0) ->
    {lists:reverse(Fields), Binary};
decode_tuple(Binary, Fields, Remain) ->
    {FieldSize, BinaryTail} = etarantool_varint:decode_varuint32(Binary),
    <<Field:FieldSize/binary-unit:8, BinaryTail2/binary>> = BinaryTail,
    decode_tuple(BinaryTail2, [Field|Fields], Remain - 1).

decode_tuples(Binary) ->
    <<Count:4/integer-signed-little-unit:8, BinaryTail/binary>> = Binary,
    decode_tuples(BinaryTail, Count).

decode_tuples(Binary, Count) ->
    decode_tuples(Binary, [], Count).

decode_tuples(Binary, Tuples, 0) ->
    {lists:reverse(Tuples), Binary};
decode_tuples(Binary, Tuples, Remain) ->
    {Tuple, BinaryTail} = decode_tuple(Binary),
    decode_tuples(BinaryTail, [Tuple|Tuples], Remain - 1).

decode_returncode(0) -> ok;
decode_returncode(16#00000401) -> tuple_is_ro;
decode_returncode(16#00000601) -> tuple_is_locked;
decode_returncode(16#00000701) -> memory_issue;
decode_returncode(16#00000102) -> nonmaster;
decode_returncode(16#00000202) -> illegal_params;
decode_returncode(16#00000a02) -> unsupported_command;
decode_returncode(16#00001e02) -> wrong_field;
decode_returncode(16#00001f02) -> wrong_number;
decode_returncode(16#00002002) -> duplicate;
decode_returncode(16#00002602) -> wrong_version;
decode_returncode(16#00002702) -> wal_io;
decode_returncode(Num) -> Num.
