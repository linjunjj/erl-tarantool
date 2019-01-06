-module(erl_tarantool_serialize).

-export([pack/1, pack/2]).
-include("erl_tarantool.hrl").

-type ext_packer() :: fun( (tuple(), options() )-> {ok, {Type:: byte(), Data:: binary() | {error, any()}}}).
-type ext_unpacker() :: fun((byte(), binary(), options()) -> {ok, pack_term()} | {error, any()}) | 
                        fun((byte(), binary())-> {ok, pack_term()} | {error, any()}).

-type options() ::
        [{apec, new|old} |
         {allow_atoms, none|pack} |
         {known_atoms, [atom()]} |
         {upack_str, as_binary|as_list|as_tagged_list} |
         {validate_string, boolean()} |
         {pack_str, from_binary|from_list|from_tagged_list|none} |
         {map_format, map|jiffy|jsx} |
         {ext, {ext_packer(), ext_unpacker()} | module()}].

-type opt_record() :: ?OPTION{}.
-type pack_map_unpacker() :: 
        fun((binary(), non_neg_integer(), opt_record()) -> {pack_map(), binary()} | no_return()).


-spec pack(pack_type()) ->binary() | {error, _}.
pack(Term) -> pack(Term, []).

-spec pack(pack_type(), options()) ->binary() | {error, _}.
pack(Term, Opts) ->
    ok;

%% ++++++++++++++++++++++++++++
parse_options(Opt) ->
    parse_options(Opt, ?OPTION{original_list = Opt}).

parse_options([], Opt) ->
    Opt;
parse_options([{spec, Spec} | T], Opt0) when Spec =:= new orelse Spec =:= old ->
    parse_options(Opt, Opt?OPTION{spec = Spec});

parse_options([{allow_atom, Type} | T], Opt0) ->
    Opt = case Type of
                none -> Opt0?OPTION(allow_atom = none);
                pack -> Opt0?OPTION(allow_atom=pack)
          end,
    parse_options(T, Opt);

parse_options([{}])