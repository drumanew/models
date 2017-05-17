-module (models_wrk_sup).
-behaviour (supervisor).

-export ([start_link/0]).
-export ([start_worker/1]).
-export ([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link () ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker (Args) ->
  supervisor:start_child(?MODULE, [Args]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init ([]) ->
  ChildSpecs = [{models_wrk,
                 {models_wrk, start_link, []},
                 temporary,
                 2000,
                 worker,
                 [models_wrk]}],
  {ok, {{simple_one_for_one, 1, 5}, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

