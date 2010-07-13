%%%-------------------------------------------------------------------
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
%%% File    : gdss_json_rpc_proto_server.erl
%%% Purpose : GDSS JSON-RPC protocol server
%%%-------------------------------------------------------------------

-module(gdss_json_rpc_proto_server).

-behaviour(gen_server).

%% Sample client call:
%%
%% ubf_jsonrpc_inets_httpc_simple:do(
%%   "http://localhost:7598/gdss", ubf_gdss_plugin,
%%   {do, tab1, [{get, <<"foo">>, [witness]}], [], 3333},
%%   <<"foo-json-id">>, [], [], false).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% inets HTTP server callbacks
-export([do/1]).

-define(SERVER, ?MODULE).

-record(state, {
          httpd_pid,
          port,
          uri
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port, Uri) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port, Uri], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port, Uri]) ->
    ok = application:set_env(?MODULE, port, Port),
    ok = application:set_env(?MODULE, uri, Uri),
    {ok, HttpdPid} = inets:start(httpd,
                                 [{port, Port}, {document_root, "/tmp"},
                                  {server_name, "GDSS JSON-RPC"},
                                  {server_root, "/tmp"},
                                  {modules,[?MODULE]}
                                 ]),

    {ok, #state{httpd_pid = HttpdPid, port = Port, uri = Uri}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    inets:stop(httpd, State#state.httpd_pid),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do(Info) ->
try
    {ok, Uri} = application:get_env(?MODULE, uri),
    Mod = ubf_jsonrpc_inets_httpd_simple:new(ubf_gdss_plugin, Uri, false),
    (Mod):do(Info)
catch X:Y ->
    error_logger:warning_msg("do: ~p ~p @ ~p\n", [X, Y, erlang:get_stacktrace()])
end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
