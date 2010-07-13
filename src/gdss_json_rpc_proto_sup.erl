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
%%% File    : gdss_json_rpc_proto_sup.erl
%%% Purpose : GDSS JSON-RPC top-level supervisor
%%%----------------------------------------------------------------------

-module(gdss_json_rpc_proto_sup).

-include("gdss_json_rpc_proto.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(_Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main GDSS UBF supervisor.

init(_Args) ->
    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    CJRPC = case gmt_config_svr:get_config_value_i(gdss_json_rpc_tcp_port, ?GDSS_JSON_RPC_DEFAULT_TCP_PORT) of
               {ok, 0} ->
                   [];
               {ok, JRPCPort} ->
                    {ok, JRPCUri} = gmt_config_svr:get_config_value(gdss_json_rpc_uri, ?GDSS_JSON_RPC_DEFAULT_URI),
                    JRPCServer =
                        {json_rpc_server, {gdss_json_rpc_proto_server,start_link,
                                           [JRPCPort, JRPCUri]},
                         permanent, 2000, worker, [gdss_json_rpc_proto_server]},

                    [JRPCServer]
            end,

    {ok, {{one_for_one, 2, 60}, CJRPC}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
