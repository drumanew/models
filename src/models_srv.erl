-module (models_srv).
-behaviour (gen_server).

%% API
-export ([start_link/0]).
-export ([set_data_file/1]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(state, { datafile, data }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link () -> {ok, pid()}.
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_data_file (FileName) ->
  gen_server:call(?MODULE, {set_data_file, FileName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init ([]) ->
  {ok, #state{}}.

handle_call ({set_data_file, FileName}, _From, State) ->
  {Reply, State0} = try danalys:read_data(FileName) of
                      Data -> {ok, State#state{ datafile = FileName,
                                                  data     = Data }}
                    catch
                      _:Error -> {{error, Error}, State}
                    end,
  {reply, Reply, State0};
handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

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

