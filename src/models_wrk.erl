-module (models_wrk).
-behaviour (gen_server).

%% API
-export ([start_link/1]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(state, { model, data }).

-include ("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link (Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init (Args) ->
  Model = proplists:get_value(model, Args),
  Data  = proplists:get_value(data, Args),
  gen_server:cast(self(), inited),
  {ok, #state{ model = Model, data = Data }}.

handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast (inited, State = #state{ model = Model, data = Data }) ->
  Perf = danalys:apply_model(Model, Data),
  gen_server:cast(models_srv, {report, self(), Perf}),
  {stop, normal, State};
handle_cast (_Msg, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

