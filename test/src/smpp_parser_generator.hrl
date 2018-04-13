%% -----------------------------------------------------------------------------
%%
%% smpp_parser_generator.hrl: SMPP - test data generator.
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

-ifndef(SMPP_PARSER_GENERATOR_HRL).
-define(SMPP_PARSER_GENERATOR_HRL, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("smpp_pdu.hrl").

-define(ALIVE_COUNTER, 500).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?D("~n time (ms)          ===  ~12.. B rule: ~s ~n",
        [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?D("~n memory (bytes)     ===  ~12.. B rule: ~s ~n",
        [_MemorySize, atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).

-define(FUNCTIONS, [
    encode_decode,
    pack_unpack,
    pack_unpack_map
]).

-ifdef(NODEBUG).
-define(D(Format), undefined).
-define(D(Format, Args), undefined).
-else.
-define(D(Format), ?D(Format, [])).
-define(D(Format, Args),
    io:format(user, "~p:~p:~p ===> "Format,
[?MODULE, ?FUNCTION_NAME, ?LINE | Args])).
-endif.

-define(D_CT(Format, Args),
    ct:pal(info, ?MAX_IMPORTANCE, "~p:~p:~p ===> "Format,
[?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

% TRUE: compacted / FALSE: detailed.
-define(GENERATE_COMPACTED, list_to_atom(string:to_lower(
    os:getenv("GENERATE_COMPACTED", "true")))).

-define(LOGGING, list_to_atom(string:to_lower(os:getenv("LOGGING", "false")))).

-define(MAX_BASIC, list_to_integer(os:getenv("MAX_BASIC", "50"))).
-define(MAX_DEST, 12).
-define(MAX_MESSAGE_SUBMISSION_RESPONSE_TLV, 6).
-define(MAX_MESSAGE_SUBMISSION_REQUEST_TLV, 12).
-define(MAX_OPERATION, ?MAX_BASIC * 5).
-define(MAX_UNSUCCESS, 6).

-define(PATH_CT, "test/generated/ct/").
-define(PATH_EUNIT, "test/generated/eunit/").

-define(TIMETRAP_MINUTES, 15).

-endif.