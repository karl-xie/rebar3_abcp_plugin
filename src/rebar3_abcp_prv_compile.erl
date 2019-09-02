-module(rebar3_abcp_prv_compile).
-export([init/1,
    do/1,
    format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).
-define(SHORT_DESC, "Automatically compile abcp files").
-define(DESC, "nothing").

%% ============================================================================
%% Public API
%% ============================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, abcp},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {opts, []},
        {example, "rebar3 abcp compile"},
        {short_desc, ?SHORT_DESC},
        {desc, ?DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
        end,
    lists:foreach(fun(App) ->
        rebar3_abcp:compile(App, State)
    end, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).