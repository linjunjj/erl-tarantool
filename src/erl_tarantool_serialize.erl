-module(erl_tarantool_serialize).
-export([pack/1, pack/2]).
-type pack_map_jsx() :: [{pack_term(), pack_term()} | [{}]].
-type pack_map_jiffy() :: {[{pack_term(), pack_term()}]}.
-type pack_map() :: pack_map_jsx() | pack_map_jiffy() | map().
-type pack_term() :: [pack_term()] | pack_map() | integer() | float() | boolean() | binary() | string() | {string, string()}.
-type options() ::
        [{apec, new|old} |
         {allow_atoms, none|pack} |
         {known_atoms, [atom()]} |
         {upack_str, as_binary|as_list|as_tagged_list} |
         {validate_string, boolean()} |
         {pack_str, form_binary|from_list|from_tagged_list|none} |
         {map_format, map|jiffy|jsx} |
         {ext, {}}].
-type ext_packer() :: fun( (tuple(), options() )-> {ok, {Type:: byte(), Data:: binary() | {error, any()}}}).
-type format_type() :: jsx|jiffy|map.




-spec pack(pack_type(), )