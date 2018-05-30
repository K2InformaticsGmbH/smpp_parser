-module(interface_unit_test).
-include_lib("eunit/include/eunit.hrl").

-include("tests.hrl").

packunpack_test_() ->
    {inparallel,
     [{T, fun() ->
            Bin = list_to_binary([binary_to_integer(B, 16) || B <- re:split(L, " ")]),
            SMPP = smpp:unpack_map(Bin),
            E1 = smpp:to_enum(smpp:internal2json(SMPP)),
            if E /= E1 ->
                    ?debugFmt("~n~s~nExpected : ~p~n"
                                "Got      : ~p", [T, E, E1]);
                true -> ok
            end,
            ?assertEqual(E, E1),
            {ok, NewBin} = smpp:pack(SMPP),
            NewSMPP = smpp:unpack_map(NewBin),
            if NewSMPP /= SMPP ->
                    ?debugFmt("~n~s~n"
                              "Expected : ~p~n"
                              "Got      : ~p~n",
                              [T, SMPP, NewSMPP]);
                true -> ok
            end,
            ?assertEqual(SMPP, NewSMPP)
          end}
      || {T,L,E} <- ?TESTS]
    }.

-define(PDU(_Id,_Extra), <<"{\"command_id\":",(integer_to_binary(_Id))/binary,
                           ",\"command_status\":0,\"sequence_number\":0",
                           _Extra,"}">>).
-define(PDU(_Id), ?PDU(_Id, "")).
-define(PDU_SYSID(_Id), ?PDU(_Id, ",\"system_id\":\"\",\"sc_interface_version\":80")).
-define(PDU_DSTADDR(_Id), ?PDU(_Id, ",\"destination_addr\":\"\"")).
-define(TESTS2,
[% requests
{"bind_receiver",          16#00000001,  ?PDU_SYSID(16#00000001)},
{"bind_transmitter",       16#00000002,  ?PDU_SYSID(16#00000002)},
{"query_sm",               16#00000003,  ?PDU_DSTADDR(16#00000003)},
{"submit_sm",              16#00000004,  ?PDU_DSTADDR(16#00000004)},
{"deliver_sm",             16#00000005,  ?PDU_DSTADDR(16#00000005)},
{"unbind",                 16#00000006,  ?PDU_DSTADDR(16#00000006)},
{"replace_sm",             16#00000007,  ?PDU_DSTADDR(16#00000007)},
{"cancel_sm",              16#00000008,  ?PDU_DSTADDR(16#00000008)},
{"bind_transceiver",       16#00000009,  ?PDU_SYSID(16#00000009)},
{"outbind",                16#0000000B,  ?PDU_SYSID(16#0000000B)},
{"enquire_link",           16#00000015,  ?PDU(16#000000015)},

% responses
{"bind_receiver_resp",     16#80000001,  ?PDU_SYSID(16#80000001)},
{"bind_transmitter_resp",  16#80000002,  ?PDU_SYSID(16#80000002)},
{"query_sm_resp",          16#80000003,  ?PDU(16#80000003)},
{"submit_sm_resp",         16#80000004,  ?PDU(16#80000004)},
{"deliver_sm_resp",        16#80000005,  ?PDU(16#80000005)},
{"unbind_resp",            16#80000006,  ?PDU(16#80000006)},
{"replace_sm_resp",        16#80000007,  ?PDU(16#80000007)},
{"cancel_sm_resp",         16#80000008,  ?PDU(16#80000008)},
{"bind_transceiver_resp",  16#80000009,  ?PDU_SYSID(16#80000009)},
{"enquire_link_resp",      16#80000015,  ?PDU(16#80000015)}
]).

json_pack_test_() ->
    {inparallel,
        [{T,
            fun() ->
                I = smpp:json2internal(jsx:decode(J, [return_maps])),
                case smpp:pack(I) of
                    {error, _ , Error, _} ->
                        ?assertEqual(ok, smpp:err(Error));
                    {ok, Bin} ->
                        ?assertMatch(<<_:32/integer,C:32/integer,_/binary>>, Bin),
                        % 2nd pass (complete PDU support test)
                        J1 = jsx:encode(smpp:internal2json(smpp:unpack_map(Bin))),
                        smpp:json2internal(jsx:decode(J1, [return_maps]))
                end
            end}
        || {T,C,J} <- ?TESTS2]
    }.

encode_decode_test_() ->
    {inparallel,
        [{T,
            fun() ->
                {ok, D} = smpp:decode(P),
                ?assertEqual(Ex, D),
                ?assertEqual(true, is_map(D)),
                ?assertEqual({ok, D}, smpp:decode(re:replace(P,"\s","",[global,{return,list}]))),
                ?assertEqual({ok, D}, smpp:decode(re:replace(P,"\s","",[global,{return,binary}]))),
                {ok, E} = smpp:encode(jsx:decode(jsx:encode(D), [return_maps])),
                ?assertEqual(true, is_binary(E)),
                ?assertEqual({ok, D}, smpp:decode(E))
            end}
        || {T,P,Ex} <- ?TESTS]
    }.

encode_decode_1_test_() ->
    {inparallel,
        [{T,
            fun() ->
                I = smpp:json2internal(jsx:decode(J, [return_maps])),
                case smpp:pack(I) of
                    {error, _ , Error, _} ->
                        ?assertEqual(ok, smpp:err(Error));
                    {ok, Bin} ->
                        ?assertMatch(<<_:32/integer,C:32/integer,_/binary>>, Bin),
                        {ok, D} = smpp:decode(
                                    <<<<(list_to_binary(
                                            string:right(
                                                integer_to_list(B,16),2,$0)
                                        ))/binary>>||<<B>><=Bin>>),
                        ?assertEqual(true, is_map(D))
                end
            end}
        || {T,C,J} <- ?TESTS2]
    }.

enum_test_() ->
    {inparallel,
        [{T,
            fun() ->
                I = smpp:json2internal(jsx:decode(J, [return_maps])),
                {ok, Bin} = smpp:pack(I),
                #{} = D = smpp:unpack_map(Bin),
                S = jsx:decode(jsx:encode(D), [return_maps]),
                #{<<"command_id">> := CmdId,
                  <<"command_status">> := CommandStatus} = S1 = smpp:to_enum(S),
                ?assertEqual(true, is_binary(CmdId)),
                ?assertEqual(true, is_binary(CommandStatus)),
                ?assertEqual(S, smpp:from_enum(S1))
            end}
        || {T,_,J} <- ?TESTS2]
    }.


templates_test_() ->
    #{templates := Templates} = smpp:info(),
    {inparallel,
        maps:fold(
            fun(T, Pdu, Acc) ->
                [{atom_to_list(T),
                  fun() ->
                    case smpp:encode(Pdu) of
                        {error, _ , Error, _} ->
                            ?assertEqual(ok, smpp:err(Error));
                        {ok, Bin} ->
                            {ok, Pdu2} = smpp:decode(Bin),
                            ?assertEqual(0, length(maps:keys(Pdu) -- maps:keys(Pdu2)))
                    end
                  end} | Acc]
            end, [], Templates)
    }.

-define(IGNORE_FIELDS,
    [schedule_delivery_time, service_type, short_message, sm_default_msg_id,
     source_addr, source_addr_npi, source_addr_ton, validity_period,
     data_coding, dest_addr_npi, dest_addr_ton, destination_addr, esm_class,
     priority_flag, protocol_id, registered_delivery, replace_if_present_flag,
     command_length, command_status, sequence_number]).

vendor_tlv_test_() ->
    {inparallel,
        [{T,
            fun() ->
                 {ok, D} = smpp:decode(I),
                 ?assertEqual(O, maps:without(?IGNORE_FIELDS, D)),
                 {ok, E} = smpp:encode(jsx:decode(jsx:encode(D), [return_maps])),
                 ?assertEqual(true, is_binary(E)),
                 {ok, D1} = smpp:decode(E),
                 ?assertEqual(O, maps:without(?IGNORE_FIELDS, D1))
            end
         } || {T,I,O} <-
            [
                {"submit_sm",
                 "00 00 00 31 00 00 00 04 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
                 "02 04 00 01 01 " % user_message_reference
                 "14 00 00 01 02 "
                 "14 01 00 02 03 04",
                 #{command_id => <<"submit_sm">>,
                   user_message_reference => 1,
                   tlvs => [#{tag => 16#1400, len => 1, val => <<2>>},
                            #{tag => 16#1401, len => 2, val => <<3,4>>}]
                  }
                },
                {"submit_sm-1",
                 "00 00 00 31 00 00 00 04 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
                 "14 01 00 02 03 04"
                 "14 00 00 01 02 "
                 "02 04 00 01 01 ", % user_message_reference
                 #{command_id => <<"submit_sm">>,
                   user_message_reference => 1,
                   tlvs => [#{tag => 16#1401, len => 2, val => <<3,4>>},
                            #{tag => 16#1400, len => 1, val => <<2>>}]
                  }
                },
                {"submit_sm-2",
                 "00 00 00 31 00 00 00 04 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
                 "14 00 00 01 02 "     % {5120,1,[2]}
                 "14 01 00 02 03 04 "  % {5121,2,[3,4]}
                 "14 02 00 01 05",    % {5122,1,[5]},
                 #{command_id => <<"submit_sm">>,
                   tlvs => [#{tag => 16#1400, len => 1, val => <<2>>},
                            #{tag => 16#1401, len => 2, val => <<3,4>>},
                            #{len => 1,tag => 5122,val => <<5>>}]
                  }
                }

            ]
        ]
    }.

schema_test() ->
    #{schema := Schema} = smpp:info(),
    ?assertEqual(true, is_binary(jsx:encode(Schema))).