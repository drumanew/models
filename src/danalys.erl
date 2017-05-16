-module (danalys).
-compile(export_all).

-define (LOG (F, A),
         io:format(standard_error, "[~s:~b] " ++ F ++ "~n", [?MODULE, ?LINE | A])).
-define (LOG (What), ?LOG(What, [])).

read_data (Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Strings = binary_to_list(Binary),
  [ _ | Lines ] = string:tokens(Strings, "\r\n"),
  AllData = [ list_to_tuple(string:tokens(L, ",")) || L <- Lines ],
  [ { Date, list_to_float(Open), list_to_float(Close) }
    || {Date, Open, _, _, Close, _} <- AllData ].

format_data (Data) ->
  [ io:format("~s | ~f | ~f~n", tuple_to_list(Chunk)) || Chunk <- Data ].

rand_walk_data (Data) ->
  lists:mapfoldl(fun ({Date, Open, Close}, Sum) ->
                   Good = case Close > Open of
                            true -> '+';
                            _    -> '-'
                          end,
                   {Do, Diff} = case rand:uniform(5) of
                                  1 -> {'+', Close - Open};
                                  2 -> {'++', 2*(Close - Open)};
                                  3 -> {'-', Open - Close};
                                  4 -> {'--', 2*(Open - Close)};
                                  5 -> {'+-', 0.0}
                                end,
                   Sign = case Diff >= 0 of
                            true -> "+";
                            _    -> ""
                          end,
                   {{Date, Open, Close, Good, Do, Sign, Diff}, Sum + Diff}
                 end, 0, Data).

format_walked_data ({Data, {Sum, _}}) ->
  Sign = case Sum >= 0 of
           true -> "+";
           _    -> ""
         end,
  io:format("total: ~s~f~n", [Sign, Sum]),
  [ io:format("~s | ~f | ~f | ~s | ~2s | ~s~f~n", tuple_to_list(Chunk))
    || Chunk <- Data ].

gen_models (Dir, Count, Length) ->
  case filelib:ensure_dir(Dir) of
    ok   -> gen_model(Dir, Count, Length);
    Else -> throw(Else)
  end.

gen_model (_, 0, _) ->
  ok;
gen_model (Dir, Count, Length) when is_integer(Count) andalso
                                    Count > 0 andalso
                                    is_integer(Length) andalso
                                    Length > 0 ->
  Filename = lists:flatten(io_lib:format("model_~10..0b", [Count])),
  Data =
    lists:map(fun (_) ->
                Step =
                  case rand:uniform(5) of
                    1 -> "+";
                    2 -> "++";
                    3 -> "-";
                    4 -> "--";
                    5 -> "+-"
                  end,
                io_lib:format("yyyy/mm/dd,~s~n", [Step])
              end, lists:seq(1, Length)),
  {ok, Fd} = file:open(Dir ++ "/" ++ Filename, write),
  file:write(Fd, Data),
  file:close(Fd),
  gen_model (Dir, Count - 1, Length).

read_model (Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Strings = binary_to_list(Binary),
  Lines = string:tokens(Strings, "\r\n"),
  AllData = [ list_to_tuple(string:tokens(L, ",")) || L <- Lines ],
  [ {Date, case Step of
             "+"  -> 1;
             "++" -> 2;
             "-"  -> 3;
             "--" -> 4;
             "+-" -> 5
           end} || {Date, Step} <- AllData ].

apply_model (Model, Data) when length(Model) == length(Data) ->
  {Perf, _} =
    lists:foldl(fun ({_Date, Open, Close}, {Sum, [{_, Step} | Rest]}) ->
                     Diff = case Step of
                              1 -> Close - Open;
                              2 -> 2*(Close - Open);
                              3 -> Open - Close;
                              4 -> 2*(Open - Close);
                              5 -> 0.0
                            end,
                     {Sum + Diff, Rest}
                   end, {0, Model}, Data),
  Perf.

perf_test () ->
  perf_test([]).

perf_test (Opts) ->
  try
    ModelsCount = getoptarg(count, Opts, 100),
    NoGenerate = getopt(no_gen, Opts, false),
    OnlyGenerate = getopt(only_gen, Opts, false),
    NoClear = getopt(no_clear, Opts, false) orelse NoGenerate,
    OnlyClear = getopt(only_clear, Opts, false),
    case OnlyGenerate andalso OnlyClear of
      true -> throw("Ambiguous options");
      _    -> ok
    end,
    ?LOG("==== PERFORMANCE TEST STARTED ===="),
    case NoClear of
      true -> ok;
      _ ->
        ?LOG("Clear previously generated model(s)..."),
        os:cmd("rm -f /tmp/models/model_*")
    end,
    case OnlyClear of
      true -> throw(stop);
      _    -> ok
    end,
    ?LOG("Reading CSV data..."),
    Data = read_data("example.csv"),
    Length = length(Data),
    case NoGenerate of
      true -> ok;
      _ ->
        ?LOG("Generating ~b model(s)...", [ModelsCount]),
        gen_models("/tmp/models/", ModelsCount, Length)
    end,
    case OnlyGenerate of
      true -> throw(stop);
      _    -> ok
    end,
    ?LOG("Start applying model(s) at ~s", [ts()]),
    {ok, Files} = file:list_dir("/tmp/models"),
    {OK, Bad, TheWorst, TheBest} =
      lists:foldl(fun
                    (File = "model_" ++ _, {OKAcc, BadAcc, Worst, Best}) ->
                      Prof = apply_model(read_model("/tmp/models/" ++ File), Data),
                      {NewOKAcc, NewBadAcc} =
                        case Prof > 0 of
                          true -> {OKAcc + 1, BadAcc};
                          _    -> {OKAcc, BadAcc + 1}
                        end,
                      NewWorst =
                        case Worst == undefined orelse Prof < Worst of
                          true -> Prof;
                          _    -> Worst
                        end,
                      NewBest =
                        case Best == undefined orelse Prof > Best of
                          true -> Prof;
                          _    -> Best
                        end,
                      {NewOKAcc, NewBadAcc, NewWorst, NewBest};
                    (_, Acc) -> Acc
                  end, {0, 0, undefined, undefined}, Files),
    Fmt = fun
            (undefined)    -> "n/a";
            (W) when W > 0 -> io_lib:format("+~f", [W]);
            (W)            -> io_lib:format("~f", [W])
          end,
    ?LOG("Applying done at ~s", [ts()]),
    ?LOG("Summary:"),
    ?LOG("  good model(s): ~b", [OK]),
    ?LOG("  bad model(s) : ~b", [Bad]),
    ?LOG("  best model   : ~s", [Fmt(TheBest)]),
    ?LOG("  worst model  : ~s", [Fmt(TheWorst)])
  catch
    throw:stop  -> ok;
    Class:Error -> apply(erlang, Class, [Error])
  after
    ?LOG("==== PERFORMANCE TEST DONE ===="),
    ok
  end.

ts () ->
  {{YY, MM, DD}, {H, M, S}} = calendar:now_to_local_time(erlang:timestamp()),
  lists:flatten(
    io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
                  [YY, MM, DD, H, M, S])).

getoptarg (Opt, Proplist, Default) ->
  proplists:get_value(Opt, Proplist, Default).

getopt (Opt, List, Default) ->
  lists:member(Opt, List) orelse Default.
