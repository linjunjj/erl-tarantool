-module(erl_tarantool_serialize).

-export([pack/1, pack/2]).
-include("erl_tarantool.hrl").

-type ext_packer() :: fun( (tuple(), options() )-> {ok, {Type:: byte(), Data:: binary() | {error, any()}}}).
-type ext_unpacker() :: fun((byte(), binary(), options()) -> {ok, pack_term()} | {error, any()}) | 
                        fun((byte(), binary())-> {ok, pack_term()} | {error, any()}).

-type opt_record() :: ?OPTION{}.
-type pack_map_unpacker() :: 
        fun((binary(), non_neg_integer(), opt_record()) -> {pack_map(), binary()} | no_return()).
-type format_type() :: jsx | jiffy | map.
-define(DEFAULT_MAP_UNPACKER_FUN, fun unpack_map/3).

-spec pack(pack_type()) ->binary() | {error, _}.
pack(Term) -> pack(Term, []).

-spec pack(pack_type(), options()) ->binary() | {error, _}.
pack(Term, Opts) ->
    Option = 


parse_options(Opt) ->
    parse_options(Opt, )