-module (models_srv).
-behaviour (gen_server).

%% API
-export ([start_link/0]).
-export ([set_data_file/1,
          get_data_file/0,

          set_max_mem_size/1,
          set_max_mem_size/2,
          get_max_mem_size/0,
          reset_max_mem_size/0,

          set_max_workers/1,
          get_max_workers/0,
          reset_max_workers/0,

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

-define (MAX_MEM_SIZE_DEF, 10*1024*1024). %% 100 MBytes
-define (MAX_WORKERS_DEF, 10).

-record(state, { datafile,
                 data,

                 max_size    = ?MAX_MEM_SIZE_DEF,
                 max_workers = ?MAX_WORKERS_DEF,

                 pending_dirs  = gb_sets:new(),
                 pending_files = gb_sets:new(),

                 loaded_data      = gb_trees:empty(),
                 loaded_data_size = 0,

                 timer }).

-include ("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link () -> {ok, pid()}.
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------

set_data_file (FileName) when is_list(FileName) ->
  gen_server:call(?MODULE, {set_data_file, FileName}).

get_data_file () ->
  gen_server:call(?MODULE, get_data_file).

%---------------------------------------------------------------------

set_max_mem_size (Bytes) when is_integer(Bytes) andalso Bytes > 0 ->
  gen_server:call(?MODULE, {set_max_mem_size, Bytes}).

set_max_mem_size (Count, 'b') ->
  set_max_mem_size(Count);
set_max_mem_size (Count, 'kb') ->
  set_max_mem_size(Count * 1024);
set_max_mem_size (Count, 'mb') ->
  set_max_mem_size(Count * 1024 * 1024);
set_max_mem_size (Count, 'gb') ->
  set_max_mem_size(Count * 1024 * 1024 * 1024).

get_max_mem_size () ->
  gen_server:call(?MODULE, get_max_mem_size).

reset_max_mem_size () ->
  set_max_mem_size(?MAX_MEM_SIZE_DEF).

%---------------------------------------------------------------------

set_max_workers (Count) when is_integer(Count) andalso Count > 0 ->
  gen_server:call(?MODULE, {set_max_workers, Count}).

get_max_workers () ->
  gen_server:call(?MODULE, get_max_workers).

reset_max_workers () ->
  set_max_workers(?MAX_WORKERS_DEF).

%---------------------------------------------------------------------

add_models_dir (DirName) when is_list(DirName) ->
  gen_server:call(?MODULE, {add_models_dir, DirName}).

add_model (FileName) when is_list(FileName) ->
  gen_server:call(?MODULE, {add_model, FileName}).

add_models (WildCard) when is_list(WildCard) ->
  Files = gb_sets:from_list(filelib:wildcard(WildCard)),
  gen_server:call(?MODULE, {add_models, Files}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init ([]) ->
  State = start_timer(#state{}),
  {ok, State}.

%---------------------------------------------------------------------

handle_call ({set_data_file, FileName}, _From, State) ->
  {Reply, State0} = try danalys:read_data(FileName) of
                      Data -> {ok, State#state{ datafile = FileName,
                                                data     = Data }}
                    catch
                      _:Error -> {{error, Error}, State}
                    end,
  {reply, Reply, State0};
handle_call (get_data_file, _From, State = #state{ datafile = FileName }) ->
  {reply, FileName, State};

%---------------------------------------------------------------------

handle_call ({set_max_mem_size, Bytes}, _From, State) ->
  {reply, ok, State#state{ max_size = Bytes }};
handle_call (get_max_mem_size, _From, State = #state{ max_size = Bytes }) ->
  {reply, Bytes, State};

%---------------------------------------------------------------------

handle_call ({set_max_workers, Count}, _From, State) ->
  {reply, ok, State#state{ max_workers = Count }};
handle_call (get_max_workers, _From, State = #state{ max_workers = Count }) ->
  {reply, Count, State};

%---------------------------------------------------------------------

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
             State = #state{ pending_files = PF }) ->
  {Reply, State0} =
    case filelib:is_regular(FileName) of
      true -> {ok, State#state{ pending_files = gb_sets:add(FileName, PF) }};
      _    -> {{error, enoent}, State}
    end,
  {reply, Reply, State0};
handle_call ({add_models, Files},
             _From,
             State = #state{ pending_files = PF }) ->
  State0 = State#state{ pending_files = gb_sets:union(Files, PF) },
  {reply, ok, State0};

%---------------------------------------------------------------------

handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

%---------------------------------------------------------------------

handle_cast (_Msg, State) ->
  {noreply, State}.

%---------------------------------------------------------------------

handle_info ({timeout, Timer, update}, State = #state{ timer = Timer }) ->
  State0 = reshedule(State),
  {noreply, State0};
%---------------------------------------------------------------------

handle_info (_Info, State) ->
  {noreply, State}.

%---------------------------------------------------------------------

terminate (_Reason, _State) ->
  ok.

%---------------------------------------------------------------------

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_timer (State = #state{ timer = undefined }) ->
  State#state{ timer = erlang:start_timer(1000, self(), update) };
start_timer (State = #state{ timer = Timer }) ->
  erlang:cancel_timer(Timer),
  start_timer(State#state{ timer = undefined }).

%---------------------------------------------------------------------

reshedule (State) ->
  State0 = read_dirs(State),
  State1 = read_files(State0),
  State2 = update_workers(State1),
  start_timer(State2).

%---------------------------------------------------------------------

read_dirs (State = #state{ pending_dirs = PDirs,
                           pending_files = PFiles }) ->
  PF = gb_sets:fold(fun (Dir, Files) ->
                      case file:list_dir(Dir) of
                        {ok, FileList} ->
                          gb_sets:union(Files, gb_sets:from_list(FileList));
                        _ -> Files
                      end
                    end, PFiles, PDirs),
  State#state{ pending_dirs = gb_sets:new(), pending_files = PF }.

%---------------------------------------------------------------------

read_files (State = #state{ loaded_data_size = S, max_size = M }) when S >= M ->
  State;
read_files (State = #state{ pending_files = {0, nil} }) ->
  State;
read_files (State = #state{ loaded_data_size = S,
                            loaded_data = LD,
                            max_size = M,
                            pending_files = PF }) ->
  {File, PF0} = gb_sets:take_smallest(PF),
  Model = danalys:read_model(File),
  NewSize = size(term_to_binary(Model)) + S,
  case NewSize > M of
    true -> State;
    _ ->
      read_files(State#state{ loaded_data_size = NewSize,
                              loaded_data = gb_trees:insert(File, Model, LD),
                              pending_files = PF0 })
  end.

%---------------------------------------------------------------------

update_workers (State) ->
  State.
