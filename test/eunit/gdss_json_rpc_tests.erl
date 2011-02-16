%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File    : gdss_json_rpc_tests.erl
%%% Purpose :
%%%----------------------------------------------------------------------

-module(gdss_json_rpc_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("gdss_json_rpc_proto.hrl").
-include("ubf_gdss_plugin.hrl").

-define(MY_ID, <<"foo_id_thingie">>).


all_tests_test_() ->
    {setup,
     fun test_setup/0,
     fun test_teardown/1,
     fun(_) ->[
               %% Basics
               ?_test(do_description()),
               ?_test(do_info()),
               ?_test(do_keepalive()),

               %% Negative basics
               ?_test(do_negatives()),

               %% Very very basic
               ?_test(do_very_very_basic())
              ]
     end
    }.

test_setup() ->
    ubf_gdss_eunit_utils:simple_internal_setup(),
    ubf_gdss_eunit_utils:simple_hard_reset(),
    application:start(gdss_ubf_proto),
    application:start(gdss_json_rpc_proto),
    ok.

test_teardown(_) ->
    application:stop(gdss_json_rpc_proto),
    application:stop(gdss_ubf_proto),
    ubf_gdss_eunit_utils:simple_internal_teardown(),
    ok.

do_description() ->
    {ok, ?S(Desc), undefined, ?MY_ID} = json_rpc(description),
    true = is_list(Desc),
    ok.

do_info() ->
    {ok, ?S(Info), undefined, ?MY_ID} = json_rpc(info),
    true = is_list(Info),
    ok.

do_keepalive() ->
    {ok, ok, undefined, ?MY_ID} = json_rpc(keepalive),
    ok.

do_negatives() ->
    F = fun(X) ->
                {ok, undefined, Errs, ?MY_ID} = json_rpc(X),
                true = is_binary(proplists:get_value(name, Errs)),
                102 = proplists:get_value(code, Errs),
                true = is_binary(proplists:get_value(message, Errs))
        end,
    lists:foreach(F, [foo, bar, {baz, asdf}]),
    ok.

do_very_very_basic() ->
    %% Positive
    {ok, [], undefined, ?MY_ID} = json_rpc({do, tab1, [], [], 4444}),
    BadKey = <<"asdfasdflkjsadflkjasdflkjasdflkjasdflkjasdf">>,
    {ok, [key_not_exist], undefined, ?MY_ID} =
        json_rpc({do, tab1, [{get, BadKey, [witness]}], [], 4444}),

    %% Negative
    {ok, undefined, _, ?MY_ID} = json_rpc({do, tab1, [sadf], [], 4444}),
    {ok, undefined, _, ?MY_ID} = json_rpc({do, tab1, [], [qweroi], 4444}),

    ok.

json_rpc(Query) ->
    ubf_jsonrpc_inets_httpc_simple:do(make_url(), ubf_gdss_plugin,
                                      Query, ?MY_ID, [], [], false).


make_url() ->
    "http://localhost:" ++ integer_to_list(?GDSS_JSON_RPC_DEFAULT_TCP_PORT) ++
        ?GDSS_JSON_RPC_DEFAULT_URI.
