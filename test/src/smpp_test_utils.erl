%% -----------------------------------------------------------------------------
%%
%% smpp_test_utils.erl: SMPP - test driver utilities.
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

-module(smpp_test_utils).

-export([
    encode_decode/1,
    pack_unpack/1,
    pack_unpack_map/1
]).

-define(NODEBUG, true).

-include("smpp_parser_generator.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test: smpp:decode & smpp:encode.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_decode({Command, PDU}) ->
    ?D_CT("Start~nCommand: ~p~nPDU: ~p~n", [Command, PDU]),
    {ok, D} = smpp:decode(PDU),
    ?assertEqual(true, is_map(D)),
    ?assertEqual({ok, D},
        smpp:decode(re:replace(PDU, "\s", "", [global, {return, list}]))),
    ?assertEqual({ok, D},
        smpp:decode(re:replace(PDU, "\s", "", [global, {return, binary}]))),
    {ok, E} = smpp:encode(jsx:decode(jsx:encode(D), [return_maps])),
    ?assertEqual(true, is_binary(E)),
    ?assertEqual({ok, D}, smpp:decode(E)).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test: smpp:pack & smpp:unpack.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_unpack({Command, PDU}) ->
    ?D_CT("Start~nCommand: ~p~nPDU: ~p~n", [Command, PDU]),
    Bin = list_to_binary([binary_to_integer(B, 16) || B <- re:split(PDU, " ")]),
    SMPP = lists:foldl(fun smpp:list_to_map/2, #{}, smpp:unpack(Bin)),
    JSON = smpp:internal2json(SMPP),
    ?D_CT("~s~n~p~n~s~n", [Command, SMPP, jsx:prettify(jsx:encode(JSON))]),
    {ok, NewBin} = smpp:pack(SMPP),
    if Bin /= NewBin ->
        ?D_CT("~nExpected : ~p~nGot      : ~p", [Bin, NewBin]);
        true -> ok
    end,
    ?assertEqual(Bin, NewBin).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test: smpp:pack & smpp:unpack_map.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pack_unpack_map({Command, PDU}) ->
    ?D_CT("Start~nCommand: ~p~nPDU: ~p~n", [Command, PDU]),
    Bin = list_to_binary([binary_to_integer(B, 16) || B <- re:split(PDU, " ")]),
    SMPP = smpp:unpack_map(Bin),
    JSON = smpp:internal2json(SMPP),
    ?D_CT("~s~n~p~n~s~n", [Command, SMPP, jsx:prettify(jsx:encode(JSON))]),
    {ok, NewBin} = smpp:pack(SMPP),
    if Bin /= NewBin ->
        ?D_CT("~nExpected : ~p~nGot      : ~p", [Bin, NewBin]);
        true -> ok
    end,
    ?assertEqual(Bin, NewBin).
