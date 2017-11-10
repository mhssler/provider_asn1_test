-module(provider_asn1_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,

         use_asn1_args_from_top_rebar_config/1]).

all() ->
    [{group, one_app},
     {group, many_apps}].

all_tcs() ->
    [use_asn1_args_from_top_rebar_config].

groups() ->
    [{one_app, [], all_tcs()},
     {many_apps, [], all_tcs()}].

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------
init_per_group(Group, Config) ->
    DataDir = ?config(data_dir, Config),
    ReleaseDir = filename:join(DataDir, Group),
    [{release_dir, ReleaseDir}].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ReleaseDir = ?config(release_dir, Config),
    {ok, _} = rebar_utils:sh("rebar3 clean", [{cd, ReleaseDir}]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

use_asn1_args_from_top_rebar_config(Config) ->
    %% Configure
    ReleaseDir = ?config(release_dir, Config),
    AppEbinDir = filename:join([ReleaseDir, "_build", "default", "lib",
                                "app", "ebin"]),

    TopRebarConfigFile = filename:join(ReleaseDir, "rebar.config"),
    TopRebarConfigTxt =
        "{erl_opts, [debug_info]}.\n"
        "{deps, []}.\n"
        "{plugins, [{provider_asn1,\n"
        "   {git, \"https://github.com/mhssler/provider_asn1.git\",\n"
        "    {tag, \"master\"}}}]}.\n"
        "{provider_hooks, [{pre, [{compile, {asn, compile}}]},\n"
        "                  {post, [{clean, {asn, clean}}]}]}.\n"
        "{asn1_args, [{encoding, per},\n"
        "             {verbose, true},\n"
        "             {compile_opts, [der, compact_bit_string]}]}.\n",
    ok = file:write_file(TopRebarConfigFile, TopRebarConfigTxt),

    %% Generate asn1
    {ok, _} = rebar_utils:sh("rebar3 compile", [{cd, ReleaseDir}]),
    true = code:add_path(AppEbinDir),
    %% TestAppName = TestAppName:module_info(module),

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
