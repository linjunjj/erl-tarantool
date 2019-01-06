-type pack_map_jsx() :: [{pack_term(), pack_term()} | [{}]].
-type pack_map_jiffy() :: {[{pack_term(), pack_term()}]}.
-type pack_map() :: pack_map_jsx() | pack_map_jiffy() | map().
-type pack_term() :: [pack_term()] | pack_map() | integer() | float() | boolean() | binary() | string() | {string, string()}.

-type format_type() :: jsx | jiffy | map.
-define(DEFAULT_MAP_UNPACKER_FUN, fun erl_tarantool_serialize:unpack_map/3).

-record(options_v4, {
            spec = new :: new | old,
            allow_atom = pack :: none | pack,
            known_atoms = [] :: [atom() | binary()],
            unpack_str = as_list :: as_binary | as_list | as_tagged_list,
            validate_string = false :: boolean(),
            pack_str = from_list :: from_binary | from_list | from_tagged_list | none,
            map_format = map :: format_type(),
            map_unpack_fun = ?DEFAULT_MAP_UNPACKER_FUN :: pack_map_unpacker(),
            ext_packer = undefined :: erl_tarantool:ext_packer() | undefined,
            ext_unpacker = undefined :: erl_tarantool:ext_unpacker() | undefinde,
            original_list = [] :: erl_tarantool:options()
       }).
-define(OPTION, #options_v4).
