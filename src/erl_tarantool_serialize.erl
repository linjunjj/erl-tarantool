-module(erl_tarantool_serialize).

-export([pack/1, pack/2]).
-include("erl_tarantool.hrl").

-type ext_packer() :: fun( (tuple(), options() )-> {ok, {Type:: byte(), Data:: binary() | {error, any()}}}).
-type ext_unpacker() :: fun((byte(), binary(), options()) -> {ok, pack_term()} | {error, any()}) | 
                        fun((byte(), binary())-> {ok, pack_term()} | {error, any()}).

-type options() ::
        [{apec, new|old} |
         {allow_atom, none|pack} |
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
                pack -> Opt0?OPTION(allow_atom = pack)
          end,
    parse_options(T, Opt);

parse_options([{known_atoms, Atoms} | T], Opt0,) when is_list(Atoms) ->
    parse_options(T, Opt0?OPTION{known_atoms = Atoms});

parse_option([{unpack_str, As} | T], Opt0) whrn As =:= as_binary orelse As =:= as_list orelse As =:= as_tagged_list ->
    parse_option(T, Opt0?OPTION(unpack_str=As));
parse_option([{validate_string, Bool | T}], Opt0) when is_boolean(Bool) ->
    parse_option(T, Opt0?OPTION(validate_string = Bool));
parse_option([{pack_str, From} | T], Opt0) when From =:= from_binary orelse From =:= from_list orelse From =:= from_tagged_list orelse From =:= none ->
    parse_option(T, Opt0?OPTION(pack_str = From));
parse_option([{map_format, Type} | T], Opt0) when Type =:= jsx; Type =:= jiffy; Type =:= map ->
    Opt = Opt0?OPTION{map_format = Type,
                      map_unpack_fun = map_unpack_fun(Type)},
    parse_option(T, Opt?OPTION(map_format = Type));
parse_option([{ext, Module} |T ], Opt0) when is_atom(Module) ->
    Opt = Opt0?OPTION{ext_packer = fun Module:pack_ext/2,
                      ext_unpacker = fun Module:unpack_ext/3},
    parse_option(T, Opt);
parse_option([{ext, {Packer, Unpacker}} | T], Opt0) when is_function(Packer, 2) orelse is_function(Unpacker, 2) ->
    Opt = Opt0?OPTION(ext_packer= Packer, ext_unpacker = Unpacker),
    parse_option(T, Opt);
