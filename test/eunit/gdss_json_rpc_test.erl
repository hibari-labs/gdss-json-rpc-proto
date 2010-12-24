%%%----------------------------------------------------------------------
%%% Copyright: (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : gdss_json_rpc.test.erl
%%% Purpose :
%%%----------------------------------------------------------------------

-module(gdss_json_rpc_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("gdss_json_rpc_proto.hrl").
-include("ubf_gdss_plugin.hrl").

-define(MY_ID, <<"foo_id_thingie">>).

do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

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

-define(APPS, [gdss_json_rpc_proto, gdss_ubf_proto, gdss_admin, gdss_client, gdss, gmt, inets, crypto, sasl]).

test_setup() ->
    %% @TODO - boilerplate start
    os:cmd("rm -rf Schema.local hlog.* root"),
    os:cmd("ln -s ../../gdss-admin/priv/root ."),
    os:cmd("epmd -kill; sleep 1"),
    os:cmd("epmd -daemon; sleep 1"),
    {ok, _} = net_kernel:start(['eunit@localhost', shortnames]),
    [ application:stop(A) || A <- ?APPS ],
    [ ok=application:start(A) || A <- lists:reverse(?APPS) ],
    random:seed(erlang:now()),
    ok = application:set_env(gdss, brick_max_log_size_mb, 1),
    %% @TODO - boilerplate stop
    api_gdss_ubf_proto_init:simple_internal_setup(),
    api_gdss_ubf_proto_init:simple_hard_reset().

test_teardown(_) ->
    api_gdss_ubf_proto_init:simple_internal_teardown(),
    %% @TODO - boilerplate start
    [ application:stop(A) || A <- ?APPS ],
    ok = net_kernel:stop(),
    %% @TODO - boilerplate stop
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
