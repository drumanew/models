-module (models_sup).
-behaviour (supervisor).

-export ([start_link/0]).
-export ([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link () ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init ([]) ->
  Procs = [{models_wrk_sup,
            {models_wrk_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [models_wrk_sup]},
           {models_srv,
            {models_srv, start_link, []},
            permanent,
            1000,
            worker,
            [models_srv]}],
  {ok, {{one_for_one, 1, 5}, Procs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

