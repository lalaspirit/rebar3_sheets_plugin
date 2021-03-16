-module(rebar3_sheets_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_sheets_prv_clean:init(State),
    {ok, State2} = rebar3_sheets_prv_compile:init(State1),
    {ok, State2}.
