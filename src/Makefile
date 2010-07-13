###----------------------------------------------------------------------
### Copyright (c) 2007-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
###
### Licensed under the Apache License, Version 2.0 (the "License");
### you may not use this file except in compliance with the License.
### You may obtain a copy of the License at
###
###     http://www.apache.org/licenses/LICENSE-2.0
###
### Unless required by applicable law or agreed to in writing, software
### distributed under the License is distributed on an "AS IS" BASIS,
### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
### See the License for the specific language governing permissions and
### limitations under the License.
###
### File    : Makefile
### Purpose : 
###----------------------------------------------------------------------

APPNAME1 = gdss_json_rpc_proto
NODENAME1 = gdss1
MODULES1 = \
	gdss_json_rpc_proto \
	gdss_json_rpc_proto_server \
	gdss_json_rpc_proto_sup

EUNITTESTS = \
	gdss_json_rpc_test

DISABLEDQUICKTESTS = 

PLUGINS = 

# include erl make
include $(shell cat ../.depends-gmt-bom)/make/erl_make.mk

# standard targets
clean::
	rm -rf hlog.* Schema.local

check: all
	rm -rf ./*.log hlog.* Schema.local
	@echo ""
	@echo "NOTE: GDSS shutdown is extremely verbose, check exit status"
	@echo "      for real test results!"
	@echo ""
	@sleep 1
	$(RUNAPP1) -noshell -noinput \
		-s api_gdss_ubf_proto_init simple_internal_setup \
		-s gdss_json_rpc_test do_eunit \
		-s erlang halt \
                | tee -a check.log


quickcheck: all
