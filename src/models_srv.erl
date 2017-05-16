-module (models_srv).
-behaviour (gen_server).

%% API
-export ([start_link/0]).
-export ([set_data_file/1,
          add_models_dir/1,
          add_model/1,
          add_models/1]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record(state, { datafile,
                 data,
                 pending_dirs     = gb_sets:new(),
                 pending_files    = gb_sets:new(),
                 loaded_data      = [],
                 loaded_data_size = 0 }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link () -> {ok, pid()}.
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_data_file (FileName) ->
  gen_server:call(?MODULE, {set_data_file, FileName}).

add_models_dir (DirName) ->
  gen_server:call(?MODULE, {add_models_dir, DirName}).

add_model (FileName) ->
  gen_server:call(?MODULE, {add_model, FileName}).

add_models (WildCard) ->
  gen_server:call(?MODULE, {add_models, WildCard}).

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
handle_call ({add_models_dir, DirName},
             _From,
             State = #state{ pending_dirs = PD }) ->
  {Reply, State0} =
    case filelib:is_dir(DirName) of
      true -> {ok, State#state{ pending_dirs = gb_sets:add(DirName, PD) }};
      _    -> {{error, enoent}, State}
    end,
  {reply, Reply, State0};
handle_call ({add_model, FileName},
             _From,
             State = #state{ pending_files = PD }) ->
  {Reply, State0} =
    case filelib:is_regular(FileName) of
      true -> {ok, State#state{ pending_files = gb_sets:add(FileName, PD) }};
      _    -> {{error, enoent}, State}
    end,
  {reply, Reply, State0};
handle_call ({add_models, WildCard},
             _From,
             State = #state{ pending_files = PD }) ->
  Files = gb_sets:from_list(filelib:wildcard(WildCard)),
  State0 = State#state{ pending_files = gb_sets:union(Files, PD) },
  {reply, ok, State0};
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

