%%%-------------------------------------------------------------------
%% @doc erl-tarantool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_tarantool_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link("unnameed", Args) ->
    supervisor:start_link(SERVER, Args);
start_link(Name, Args) ->
    supervisor:start_link({local, Name}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args = #{cnum := CNum}) ->
    F = 
        fun(Name, Num) ->
            list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Num))
        end,

    Childs = [#{id      => F(erl_tarantool_handler, N),
                start   => {erl_traantool_handler, start_link, [Args]},
                type    => worker,
                modules => [erl_tarantool_handler]} || N <- lists:seq(1, CNum)],
    {ok, { {on_for_one, 3000, 5}, Childs} }.

%%====================================================================
%% Internal functions
%%====================================================================
