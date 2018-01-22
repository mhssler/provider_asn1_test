%%%-------------------------------------------------------------------
%%% @private
%%% @doc       Test provider_asn1
%%%
%%% Top rebar.config is <data-dir>/<group-name>/rebar.config
%%% Sub rebar.config is <data-dir>/<group-name>/lib/<app-name>/rebar.config
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(provider_asn1_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         use_asn1_args_from_top_rebar_config/1,
         use_asn1_args_from_sub_rebar_config/1,
         use_asn1_args_from_cmd_line/1]).

all() ->
    [{group, one_app},
     {group, many_apps}].

all_tcs() ->
    [use_asn1_args_from_top_rebar_config,
     use_asn1_args_from_sub_rebar_config,
     use_asn1_args_from_cmd_line].

groups() ->
    %% See SUITE data_dir (provider_asn1_SUITE_data/) for difference
    %% between the two groups.
    [{one_app, [], all_tcs()},
     {many_apps, [], all_tcs()}].

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------
init_per_group(Group, Config) ->
    DataDir = ?config(data_dir, Config),
    ReleaseDir = filename:join(DataDir, Group),
    AppDir = filename:join([ReleaseDir, "_build", "default", "lib", "app"]),
    [{release_dir, ReleaseDir},
     {app_dir, AppDir}].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ct:pal("Config:~n~p", [Config]),
    ReleaseDir = ?config(release_dir, Config),
    {ok, _} = rebar_utils:sh("rebar3 clean", [{cd, ReleaseDir}]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Top rebar config contains full configuration including
%% asn1_args. Sub rebar configs do not exist.
use_asn1_args_from_top_rebar_config(Config) ->
    ok = write_top_rebar_config(args, Config),
    ok = verify_use_of_asn1_args(Config).

%% Top rebar config minimal configuration. Sub rebar configs contains
%% asn1_args.
use_asn1_args_from_sub_rebar_config(Config) ->
    ok = write_top_rebar_config(args, Config),
    ok = write_sub_rebar_config(Config),
    ok = verify_use_of_asn1_args(Config).

%% Top rebar config contains minimal configuration. Sub rebar configs
%% do not exist. asn1_args are provided from the command line.
use_asn1_args_from_cmd_line(Config) ->
    ok = write_top_rebar_config(no_args, Config),
    CompileOpts = mk_rebar_asn1_cmd_opts(),
    CompileCmd = "rebar3 asn compile "  ++ CompileOpts,
    ok = verify_use_of_asn1_args(CompileCmd, Config),
    ok.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------
write_top_rebar_config(Type, Config) ->
    Asn1Args = case Type of
                   args ->
                       mk_rebar_config_asn1_args();
                   no_args ->
                       []
               end,
    RebarConfigTxt = mk_rebar_config_head() ++ Asn1Args,
    write_rebar_config(release_dir, RebarConfigTxt, Config).

write_sub_rebar_config(Config) ->
    RebarConfigTxt = mk_rebar_config_asn1_args(),
    write_rebar_config(app_dir, RebarConfigTxt, Config).

write_rebar_config(DirType, RebarConfigTxt, Config) ->
    AppDir = ?config(DirType, Config),
    RebarConfigFile = filename:join(AppDir, "rebar.config"),
    ct:pal("rebar.config ~p:~n~s", [RebarConfigFile, RebarConfigTxt]),
    ok = file:write_file(RebarConfigFile, RebarConfigTxt).

mk_rebar_config_head() ->
    "{erl_opts, [debug_info]}.\n"
        "{deps, []}.\n"
        "{plugins, [{provider_asn1,\n"
        "   {git, \"https://github.com/mhssler/provider_asn1.git\",\n"
        "    {tag, \"master\"}}}]}.\n"
        "{provider_hooks, [{pre, [{compile, {asn, compile}}]},\n"
        "                  {post, [{clean, {asn, clean}}]}]}.\n".

mk_rebar_config_asn1_args() ->
    "{asn1_args, [{encoding, per},\n"
        "             {verbose, true},\n"
        "             {compile_opts, [der, compact_bit_string]}]}.\n".

mk_rebar_asn1_cmd_opts() ->
    "-e per -v -o'der,compact_bit_string'".

verify_use_of_asn1_args(Config) ->
    verify_use_of_asn1_args("rebar3 compile", Config).

verify_use_of_asn1_args(CompileCmd, Config) ->
    ReleaseDir = ?config(release_dir, Config),
    AppDir = ?config(app_dir, Config),
    %% Generate ASN.1
    {ok, _} = rebar_utils:sh(CompileCmd, [{cd, ReleaseDir}]),
    true = code:add_path(filename:join(AppDir, "ebin")),

    %% Verify: ASN.1 module generated and compiled with the options
    %% from rebar.config
    Asn1ModuleName = 'Hello',
    Asn1ModuleName = Asn1ModuleName:module_info(module),
    Asn1ModuleAttribs = Asn1ModuleName:module_info(attributes),
    Asn1Info = proplists:get_value(asn1_info, Asn1ModuleAttribs),
    ExistingAsn1Options = proplists:get_value(options, Asn1Info),
    ExpectedAsn1Options = [per, verbose, der, compact_bit_string],

    ct:pal("Expected: ~p", [ExpectedAsn1Options]),
    ct:pal("Existing: ~p", [ExistingAsn1Options]),
    [true = lists:member(X, ExistingAsn1Options) || X <- ExpectedAsn1Options],

    ok.
