-module(rebar3_sheets_prv_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'clean').
-define(DEPS, [default, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {namespace, sheets},
    {module, ?MODULE},            % The module implementation of the task
    {bare, true},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 sheets clean"}, % How to use the plugin
    {opts, []},                   % list of options understood by the plugin
    {short_desc, "clean sheet module"},
    {desc, "clean sheet module compiled by plugin"}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Apps = case rebar_state:current_app(State) of
           undefined -> rebar_state:project_apps(State);
           AppInfo -> [AppInfo]
         end,
  lists:foreach(fun(App) ->
                  rebar3_sheets_compiler:clean(App, State)
                end, Apps),
  {ok, State}.


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).