%%
%% @copyright 2012 Roman Tsisyk
%% @author Roman Tsisyk <roman@tsisyk.com>
%%      [http://roman.tsisyk.com/]
%% 
%% @doc Tarantool/Box varuint32 (LEB128) encoding/decoding module.
%% @end
%% 
%% @reference <a href="http://en.wikipedia.org/wiki/LEB128">
%% Wikipedia Article</a>
%% 

-module(etarantool_varint).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    encode_varuint32/1,
    decode_varuint32/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Encodes `Value' into varuint32 format.
-spec encode_varuint32(pos_integer()) ->
    binary().
encode_varuint32(Value) ->
    Tail = <<0:1, Value:7/integer-unsigned>>,
    encode_varuint32_next(Value bsr 7, Tail).
encode_varuint32_next(Value, Tail)
        when Value =/= 0 ->
    Tail2 = <<1:1, Value:7/integer-unsigned, Tail/binary>>,
    encode_varuint32_next(Value bsr 7, Tail2);
encode_varuint32_next(_Value, Tail) ->
    Tail.

%% @doc Decodes `Value' from the `Binary' chunk.
%% `Tail' is the remain part of the source `Binary'.
%% @end
-spec decode_varuint32(binary()) ->
    {Value :: pos_integer(), Tail::binary()}.
decode_varuint32(Binary) ->
    decode_varuint32(Binary, 0).

decode_varuint32(<<0:1, ValuePart:7/integer-unsigned, Tail/binary>>, Value) ->
    {(Value bsl 7) bor ValuePart, Tail};
decode_varuint32(<<1:1, ValuePart:7/integer-unsigned, Tail/binary>>, Value) ->
    decode_varuint32(Tail, (Value bsl 7) bor ValuePart).
