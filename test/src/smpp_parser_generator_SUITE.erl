%% -----------------------------------------------------------------------------
%%
%% smpp_parser_generator_SUITE.erl: SMPP - test data generator.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-module(smpp_parser_generator_SUITE).

-export([
    all/0,
    end_per_suite/1,
    init_per_suite/1,
    suite/0,
    test_generate/1
]).

-include_lib("common_test/include/ct.hrl").

%%------------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%------------------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 30}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%------------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%------------------------------------------------------------------------------

all() ->
    [
        test_generate
    ].

%%------------------------------------------------------------------------------
%% TEST CASES
%%------------------------------------------------------------------------------

test_generate(_Config) ->
%%    ok = smpp_parser_generator:generate(),
    ok.
