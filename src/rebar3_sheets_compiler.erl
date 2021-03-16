%%%-------------------------------------------------------------------
%%% @author Nero.li
%%% @copyright (C) 2021, <Lilith>
%%% @doc
%%%   implements compile and clean command
%%% @end
%%% Created : 11. 三月 2021 15:39
%%%-------------------------------------------------------------------
-module(rebar3_sheets_compiler).
-author("lifan").

-export([compile/2, clean/2]).

-define(DEFAULT_OUT_ERL_DIR, "src/sheet/").

-atoms([compile]).
%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_app_info:t(),
    rebar_state:t()) -> ok.
compile(AppInfo, State) ->
  AppDir = rebar_app_info:dir(AppInfo),
  DepsDir = rebar_dir:deps_dir(State),
  Opts = rebar_app_info:opts(AppInfo),
  {ok, SheetsOpts} = dict:find(sheets_opts, Opts),
  %% check if non-recursive
  DefineFiles = proplists:get_all_values(def_file, SheetsOpts),

  TargetDir = filename:join([AppDir,
    proplists:get_value(out_dir, SheetsOpts,
      ?DEFAULT_OUT_ERL_DIR)]),

  OutModuleList = proplists:get_value(out_module_list, SheetsOpts, undefined),

  rebar_api:debug("making sure that target erl dir ~p exists", [TargetDir]),
  ok = ensure_dir(TargetDir),
  rebar_api:debug("reading define files from ~p, generating \".erl\" to ~p ",
    [DefineFiles, TargetDir]),
  %% search for define files
  FoundDefFiles = lists:foldl(fun({deps, DefFile}, Acc) ->
    Acc ++ discover(DepsDir, DefFile);
    (DefFile, Acc) ->
      Acc ++ discover(AppDir, DefFile)
                            end, [], DefineFiles),
  rebar_api:debug("sheet record define files found: ~p", [FoundDefFiles]),

  %% set the full path for the output directories
  %% add to include path dir locations of the protos
  %% remove the plugin specific options since gpb will not understand them
  SheetsOpts2 = remove_plugin_opts(target_out_dir_opt(TargetDir, SheetsOpts)),
  compile(FoundDefFiles, TargetDir, SheetsOpts2),

  case OutModuleList of
    undefined -> pass;
    FileName ->
      ModuleFiles = find_module_files(FoundDefFiles, TargetDir, SheetsOpts, []),
      FileName2 = filename:join([AppDir, FileName]),
      rebar_api:debug("write sheet module list: ~p to ~s", [ModuleFiles, FileName2]),
      write_sheet_module_file(ModuleFiles, FileName2)
  end,

  ok.

-spec clean(rebar_app_info:t(),
    rebar_state:t()) -> ok.
clean(AppInfo, State) ->
  AppDir = rebar_app_info:dir(AppInfo),
  DepsDir = rebar_dir:deps_dir(State),
  AppOutDir = rebar_app_info:out_dir(AppInfo),
  Opts = rebar_app_info:opts(AppInfo),
  {ok, SheetsOpts} = dict:find(sheets_opts, Opts),
  %% check if non-recursive
  DefineFiles = proplists:get_all_values(def_file, SheetsOpts),

  TargetErlDir = filename:join([AppOutDir,
    proplists:get_value(out_dir, SheetsOpts,
      ?DEFAULT_OUT_ERL_DIR)]),

  %% search for define files
  FoundDefFiles = lists:foldl(fun({deps, DefFile}, Acc) ->
    Acc ++ discover(DepsDir, DefFile);
    (DefFile, Acc) ->
      Acc ++ discover(AppDir, DefFile)
                              end, [], DefineFiles),
  rebar_api:debug("clean record define files found: ~p", [FoundDefFiles]),

  ModuleFiles = find_module_files(FoundDefFiles, TargetErlDir, SheetsOpts, []),
  rebar_api:debug("found module files: ~p", [ModuleFiles]),
  rebar_api:debug("deleting [~p]", [ModuleFiles]),
  rebar_file_utils:delete_each(ModuleFiles).

%% ===================================================================
%% Private API
%% ===================================================================
discover(AppDir, DefFile) -> [filename:join([AppDir, DefFile])].

compile([], _TargetDir, _SheetsOpts) -> ok;
compile([DefFile | Rest], TargetDir, SheetsOpts) ->
  ok = compile2(DefFile, TargetDir, SheetsOpts),
  compile(Rest, TargetDir, SheetsOpts).

-spec compile2(string(), string(), proplists:proplist()) -> ok.
-ifdef(OTP_RELEASE).
compile2(Source, TargetDir, SheetsOpts) ->
  rebar_api:debug("compiling ~p to ~p", [Source, TargetDir]),
  rebar_api:debug("opts: ~p", [SheetsOpts]),
  try
    CompileRet = sheets_compile:compile(Source, SheetsOpts),
    rebar_api:debug("generate module ret ~p", [CompileRet]),
    lists:foreach(
      fun
        ({ok, FileName}) -> rebar_api:debug("generate module file ~p success, source ~p", [FileName, Source]);
        ({error, {sheet_md5_same, FileName}}) -> rebar_api:debug("pass module file ~p from ~p, md5 same", [FileName, Source]);
        ({error, Error}) -> rebar_api:abort("generate module error ~p, source ~p", [Error, Source])
      end,
      CompileRet
    )
  catch
    E:R:Stack ->
      rebar_utils:abort("failed to compile ~s: ~p, ~p, ~p~n", [Source, E, R, Stack])
  end.
-else.
compile2(Source, TargetDir, SheetsOpts) ->
  rebar_api:debug("compiling ~p to ~p", [Source, TargetDir]),
  rebar_api:debug("opts: ~p", [SheetsOpts]),
  try
    CompileRet = sheets_compile:compile(Source, SheetsOpts),
    rebar_api:debug("generate module ret ~p", [CompileRet]),
    lists:foreach(
      fun
        ({ok, FileName}) -> rebar_api:debug("generate module file ~p success, source ~p", [FileName, Source]);
        ({error, {sheet_md5_same, FileName}}) -> rebar_api:debug("pass module file ~p from ~p, md5 same", [FileName, Source]);
        ({error, Error}) -> rebar_api:abort("generate module error ~p, source ~p", [Error, Source])
      end,
      CompileRet
    )
  catch
    E:R ->
      Stack = erlang:get_stacktrace(),
      rebar_utils:abort("failed to compile ~s: ~p, ~p, ~p~n", [Source, E, R, Stack])
  end.
-endif.
find_module_files([], _TargetErlDir, _SheetsOpts, Rslt) -> Rslt;
find_module_files([DefFile | Rest], TargetErlDir, SheetsOpts, Rslt) ->
  Files = find_module_files2(DefFile, TargetErlDir, SheetsOpts),
  find_module_files(Rest, TargetErlDir, SheetsOpts, Files ++ Rslt).

find_module_files2(Source, TargetDir, SheetsOpts) ->
  rebar_api:debug("find_module_files2 ~p in ~p", [Source, TargetDir]),
  sheets_compile:out_files(Source, SheetsOpts).



-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason::file:posix()}.
ensure_dir(OutDir) ->
  %% Make sure that ebin/ exists and is on the path
  case filelib:ensure_dir(filename:join(OutDir, "dummy.beam")) of
    ok -> ok;
    {error, eexist} ->
      rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
        [OutDir]);
    {error, Reason} -> {error, Reason}
  end.

-spec remove_plugin_opts(proplists:proplists()) -> proplists:proplist().
remove_plugin_opts(Opts) ->
  remove_plugin_opts(Opts, [def_file, out_module_list]).

-spec remove_plugin_opts(proplists:proplist(),
    [def_file]) -> proplists:proplist().
remove_plugin_opts(Opts, []) -> Opts;
remove_plugin_opts(Opts0, [OptToRemove | Rest]) ->
  Opts = lists:keydelete(OptToRemove, 1, Opts0),
  remove_plugin_opts(Opts, Rest).

-spec target_out_dir_opt(string(), proplists:proplist()) -> proplists:proplist().
target_out_dir_opt(Dir, Opts) ->
  lists:keystore(out_dir, 1, Opts, {out_dir, Dir}).

write_sheet_module_file(ModuleFiles, FileName) ->
  {ok, Fd} = file:open(FileName, [write, binary]),
  ModuleFiles2 = [filename:rootname(filename:basename(ModuleFile)) || ModuleFile <- ModuleFiles],
  io:format(Fd, "[\n~s\n].", [string:join(ModuleFiles2, ",\n")]),
  file:close(Fd).