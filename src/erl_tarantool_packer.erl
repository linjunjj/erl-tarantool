-module(erl_tarantool_packer).
-export([pack/2, pack_ext/3]).

-include("erl_tarantool.hrl").

-spec pack(erl_tarantool_serialize:object(), ?OPTION{}) -> binary().
pack(I, _) when is_integer(I) andalso I < 0 ->
    pack_int(I);
pack()


%+++++++
%% deal with integer data type
-spec pack_int(integer()) -> binary().
pack_int(N) when N >= -32 ->
    << 16#111:3, N:5>>;
pack_int(N) when N >= -128 ->
    <<16#D0:8, N:8/big-signed-integer-unit:1>>;
pack_int(N) when N >= -16#8000 ->
    << 16#D1:8, N:16/big-signed-integer-unit:1>>;
pack_int(N) when N >= -16#80000000 ->
    << 16#D2:8, N:32/big-signed-integer-unit:1>>;
pack_int(N) when N >= -16#800000000000 ->
    << 16#D3:8, N:64/big-signed-integer-unit:1>>;
pack_int(N) ->
    throw({badarg, N}).

%+++++++++
%% deal with uint data type
-spec pack_uint(non_neg_integer()) -> binary().
pack_uint(N) when N < 128 ->
    <<2#0:1, N:7>>;
pack_uint(N) when (N band 16#FF) =:= N ->
    <<16#CC:8, N:8>>;
pack_uint(N) when (N band 16#FFFF) =:= N ->
    <<16#CD:8, N:16/big-unsigned-integer-unit:1>>;
pack_uint(N) when (N band 16#FFFFFFFF) =:= N ->
    <<16#CE:8, N:32/big-unsigned-integer-unit:1>>;
pack_uint(N) when (N band 16#FFFFFFFFFFFFFFFF) =:=N ->
    <<16#CF:8, N:64/big-unsigned-integer-unit:1>>;
pack_uint(N) ->
    throw({badarg, N}).

%++++++++++++++
%% deal with float data type
-spec pack_double(float()) -> binary()
pack_double(F) ->
    <<16#CB:8, F:64/big-float-uint:1>>.

%+++++++++++
%% deal with raw data type
-spec pack_raw(binary()) -> binary()
pack_raw(Bin) ->
    case byte_size(Bin) of
        Len when Len < 256 ->
            <<16#C4:8, Len:8/big-unsigned-integer-unit:1, Bin/binary>>;
        Len when Len < 16#10000 ->
            <<16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary>>;
        Len when Len < 16#100000000 ->
            <<16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary>>;
        _ ->
            throw({badarg, Bin})
        end.