-module(rebar3_abcp).

-export([compile/2,
    clean/2]).

-record(rebar3_abcp_opts, {in_dir, hrl_out, erl_out}).
%% ============================================================================
%% Public Api
%% ============================================================================
-spec compile(rebar_app_info:t(), rebar_state:t()) -> ok.
compile(AppInfo, _State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    case read_abcp_opts(Opts) of
    {ok, RAO} ->
        rebar_api:debug("opts is : ~p", [RAO]),
        #rebar3_abcp_opts{
            in_dir = InDir,
            hrl_out = HrlOut,
            erl_out = ErlOut
        } = RAO,
        ensure_dir(filename:join(AppDir, HrlOut)),
        ensure_dir(filename:join(AppDir, ErlOut)),
        generate_erlang:compile(
            filename:join(AppDir, InDir), 
            filename:join(AppDir, HrlOut), 
            filename:join(AppDir, ErlOut)),
        ok;
    {false, _Error} ->
        ok
    end.

-spec clean(rebar_app_info:t(), rebar_state:t()) -> ok.
clean(AppInfo, _State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    case read_abcp_opts(Opts) of
    {ok, RAO} ->
        rebar_api:debug("opts is : ~p", [RAO]),
        #rebar3_abcp_opts{
            in_dir = InDir,
            hrl_out = HrlOut,
            erl_out = ErlOut
        } = RAO,
        Files = filelib:wildcard(filename:join([AppDir, InDir, "./**/*.abcp"])),
        lists:foreach(fun(File) ->
            Basename = filename:basename(File),
            {Head, Source, _Module} = generate_erlang:build_target_name(Basename),
            Hfile = filename:join([AppDir, HrlOut, Head]),
            Efile = filename:join([AppDir, ErlOut, Source]),
            rebar_file_utils:delete_each([Hfile, Efile])
        end, Files),
        ok;
    {false, _Error} ->
        ok
    end.

%% ============================================================================
%% Private Api
%% ============================================================================
read_abcp_opts(Opts) ->
    case dict:find(abcp_opts, Opts) of
    {ok, OptsL} ->
        {ok, read_abcp_opts_l(OptsL, default_opts())};
    error ->
        %% throw ???
        {ok, default_opts()}
    end.

read_abcp_opts_l([], R) -> R;
read_abcp_opts_l([{in_dir, V} | T], R) ->
    read_abcp_opts_l(T, R#rebar3_abcp_opts{in_dir = V});
read_abcp_opts_l([{hrl_out, V} | T], R) ->
    read_abcp_opts_l(T, R#rebar3_abcp_opts{hrl_out = V});
read_abcp_opts_l([{erl_out, V} | T], R) ->
    read_abcp_opts_l(T, R#rebar3_abcp_opts{erl_out = V});
read_abcp_opts_l([KV | T], R) ->
    rebar_api:error("unknow opts : ~p", [KV]),
    read_abcp_opts_l(T, R).

default_opts() ->
    #rebar3_abcp_opts{in_dir = "./src", 
        hrl_out = "./generate/hrl", 
        erl_out = "./generate/erl"}.

-spec ensure_dir(filelib:dirname()) -> 'ok' | {error, Reason::file:posix()}.
ensure_dir(Dir) ->
    %% Make sure that ebin/ exists and is on the path
    case filelib:ensure_dir(filename:join(Dir, "dummy.beam")) of
    ok -> ok;
    {error, eexist} ->
        rebar_utils:abort("unable to ensure dir ~p, is it maybe a broken symlink?",
        [Dir]);
    {error, Reason} -> {error, Reason}
    end.