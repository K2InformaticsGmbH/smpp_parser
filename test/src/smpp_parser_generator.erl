%% -----------------------------------------------------------------------------
%%
%% smpp_parser_generator.erl: SMPP - test data generator.
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

-module(smpp_parser_generator).

-export([
    generate/0
]).

-define(NODEBUG, true).

-define(OPERATION, [
    alert_notification,
    bind_receiver,
    bind_receiver_resp,
    bind_transceiver,
    bind_transceiver_resp,
    bind_transmitter,
    bind_transmitter_resp,
    broadcast_sm,
    broadcast_sm_resp,
    cancel_broadcast_sm,
    cancel_broadcast_sm_resp,
    cancel_sm,
    cancel_sm_resp,
    data_sm,
    data_sm_resp,
    deliver_sm,
    deliver_sm_resp,
    enquire_link,
    enquire_link_resp,
    generic_nack,
    outbind,
    query_broadcast_sm,
    query_broadcast_sm_resp,
    query_sm,
    query_sm_resp,
    replace_sm,
    replace_sm_resp,
    submit_multi,
    submit_multi_resp,
    submit_sm,
    submit_sm_resp,
    unbind,
    unbind_resp
]).

-include("smpp_parser_generator.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    ?assertEqual("00", integer_2_octet(0)),
    ?assertEqual("05", integer_2_octet(5)),
    ?assertEqual("A312", integer_2_octet(41746, 2)),
    ?assertEqual("01D95E1F", integer_2_octet(31022623, 4)),

    ?assertEqual("00", string_2_c_octet_string("")),
    ?assertEqual("48656C6C6F00", string_2_c_octet_string("Hello")),
    ?assertEqual("31323334353637383900",
        string_2_c_octet_string("123456789")),
    ?assertEqual("413246354544323738464300",
        string_2_c_octet_string("A2F5ED278FC")),

    ?assertEqual("", string_2_octet_string("")),
    ?assertEqual("48656C6C6F", string_2_octet_string("Hello")),
    ?assertEqual("313233343536373839", string_2_octet_string("123456789")),
    ?assertEqual("4132463545443237384643",
        string_2_octet_string("A2F5ED278FC")),

    erlang:display(io:format(user,
        lists:append([
            "==================================~n",
            ?MODULE_STRING,
            " : Options:~n",
            "----------------------------------~n",
            "GENERATE_COMPACTED=~p~n",
            "LOGGING           =~p~n",
            "MAX_BASIC         =~p~n",
            "==================================~n"
        ]),
        [
            ?GENERATE_COMPACTED,
            ?LOGGING,
            ?MAX_BASIC
        ])),

    ets:new(?CODE_TEMPLATES, [
        named_table,
        private,
        ordered_set
    ]),

    create_code(),

    %% Common tests ............................................................

    case ?GENERATE_COMPACTED of
        true -> ok = file_create_ct_all("compacted", ?OPERATION);
        _ -> ok = file_create_ct_all("detailed_", ?OPERATION)
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check the generated code.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_code(_Rule, []) ->
    ok;
check_code(Rule, [Head | Tail]) ->
    case Head of
        {Rule, _CommandStatus, PDUBody} ->
            ?assertEqual(0, length(PDUBody) rem 2,
                "Rule=" ++ Rule ++ " PDUBody=" ++ PDUBody);
        _ -> ?assertEqual(0, length(Head) rem 2,
            "Rule=" ++ Rule ++ " Code=" ++ Head)
    end,
    check_code(Rule, Tail).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create operation.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_byte_string([], Acc) ->
    Acc;
create_byte_string(String, []) ->
    create_byte_string(string:slice(String, 2), string:slice(String, 0, 2));
create_byte_string(String, Acc) ->
    create_byte_string(string:slice(String, 2),
        lists:append([Acc, " ", string:slice(String, 0, 2)])).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 01
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 01   <===================~n",
        [])),

    create_code(absolute_time),
    create_code(additional_status_info_text),
    create_code(addr_npi),
    create_code(addr_ton),
    create_code(address_range),
    create_code(alert_on_msg_delivery),
    create_code(billing_identification),
    create_code(broadcast_area_identifier),
    create_code(broadcast_area_success),
    create_code(broadcast_channel_indicator),
    create_code(broadcast_content_type),
    create_code(broadcast_content_type_info),
    create_code(broadcast_end_time),
    create_code(broadcast_error_status),
    create_code(broadcast_frequency_interval),
    create_code(broadcast_message_class),
    create_code(broadcast_rep_num),
    create_code(broadcast_service_group),
    create_code(callback_num_pres_ind),
    create_code(command_status),
    create_code(data_coding),
    create_code(delivery_failure_reason),
    create_code(dest_addr_np_country),
    create_code(dest_addr_np_information),
    create_code(dest_addr_np_resolution),
    create_code(dest_addr_subunit),
    create_code(dest_bearer_type),
    create_code(dest_network_type),
    create_code(dest_node_id),
    create_code(dest_port),
    create_code(destination_addr),
    create_code(display_time),
    create_code(dpf_result),
    create_code(esm_class),
    create_code(error_code),
    create_code(failed_broadcast_area_identifier),
    create_code(interface_version),
    create_code(its_reply_type),
    create_code(its_session_info),
    create_code(language_indicator),
    create_code(message_id),
    create_code(message_payload),
    create_code(message_state),
    create_code(message_state_tlv),
    create_code(more_messages_to_send),
    create_code(ms_availability_status),
    create_code(ms_msg_wait_facilities),
    create_code(ms_validity),
    create_code(network_error_code),
    create_code(network_id),
    create_code(number_of_messages),
    create_code(password),
    create_code(payload_type),
    create_code(priority_flag),
    create_code(privacy_indicator),
    create_code(protocol_id),
    create_code(qos_time_to_live),
    create_code(receipted_message_id),
    create_code(registered_delivery),
    create_code(relative_time),
    create_code(replace_if_present_flag),
    create_code(sar_msg_ref_num),
    create_code(sar_segment_seqnum),
    create_code(sar_total_segments),
    create_code(service_type),
    create_code(set_dpf),
    create_code(short_message),
    create_code(sm_default_message_id),
    create_code(sms_signal),
    create_code(source_addr_subunit),
    create_code(source_bearer_type),
    create_code(source_network_id),
    create_code(source_network_type),
    create_code(source_node_id),
    create_code(source_port),
    create_code(subaddress),
    create_code(system_id),
    create_code(system_type),
    create_code(user_message_reference),
    create_code(user_response_code),
    create_code(ussd_service_op),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 02
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 02   <===================~n",
        [])),

    create_code(broadcast_request_optional_tlvs),
    create_code(broadcast_response_optional_tlvs),
    create_code(callback_num),
    create_code(callback_num_atag),
    create_code(cancel_broadcast_optional_tlvs),
    create_code(dest),
    create_code(dest_network_id),
    create_code(dest_subaddress),
    create_code(dest_telematics_id),
    create_code(message_delivery_request_tlvs),
    create_code(message_delivery_response_tlvs),
    create_code(message_submission_request_tlvs),
    create_code(message_submission_response_tlvs),
    create_code(query_broadcast_response_optional_tlvs),
    create_code(sc_interface_version),
    create_code(source_subaddress),
    create_code(source_telematics_id),
    create_code(unsuccess),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 99   <===================~n",
        [])),

    create_code(alert_notification),
    create_code(bind_receiver),
    create_code(bind_receiver_resp),
    create_code(bind_transceiver),
    create_code(bind_transceiver_resp),
    create_code(bind_transmitter),
    create_code(bind_transmitter_resp),
    create_code(broadcast_sm),
    create_code(broadcast_sm_resp),
    create_code(cancel_broadcast_sm),
    create_code(cancel_broadcast_sm_resp),
    create_code(cancel_sm),
    create_code(cancel_sm_resp),
    create_code(data_sm),
    create_code(data_sm_resp),
    create_code(deliver_sm),
    create_code(deliver_sm_resp),
    create_code(enquire_link),
    create_code(enquire_link_resp),
    create_code(generic_nack),
    create_code(outbind),
    create_code(query_broadcast_sm),
    create_code(query_broadcast_sm_resp),
    create_code(query_sm),
    create_code(query_sm_resp),
    create_code(replace_sm),
    create_code(replace_sm_resp),
    create_code(submit_multi),
    create_code(submit_multi_resp),
    create_code(submit_sm),
    create_code(submit_sm_resp),
    create_code(unbind),
    create_code(unbind_resp),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% absolute_time
%% -----------------------------------------------------------------------------
%% final_date                                                           4.7.23.3
%% schedule_delivery_time                                               4.7.23.1
%% validity_period                                                      4.7.23.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(absolute_time = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "",
            "990101000000000+",
            "990201000000000-",
            "990310000000000-",
            "990427200000000+",
            "990526195600000-",
            "990625185545000+",
            "990724175444100-",
            "990823165343244+",
            "990922155242346-",
            "991021145141448+"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    store_code(final_date, [string_2_c_octet_string(C) || C <- Code],
        ?MAX_BASIC, false),
    store_code(schedule_delivery_time,
        [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC, false),
    store_code(validity_period, [string_2_c_octet_string(C) || C <- Code],
        ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% additional_status_info_text                                       TLV 4.8.4.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(additional_status_info_text = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "001D",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 1, 2),
                string_2_c_octet_string(Value)
            ])
            || Value <- ["my_additional_status_info_text"]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_response_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_response_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% addr_npi                                                                4.7.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(addr_npi = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            3,
            4,
            6,
            8,
            9,
            10,
            14,
            18
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% addr_ton                                                                4.7.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(addr_ton = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            4,
            5,
            6
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% address_range                                                           4.7.3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(address_range = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "127.0.0.1",
            "^1234",
            "5678$",
            "^123456$",
            "[13579]$"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% alert_notification                                          Operation 4.1.3.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(alert_notification = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{esme_addr, Esme_Addr}] = ets:lookup(?CODE_TEMPLATES, esme_addr),
    Esme_Addr_Length = length(Esme_Addr),
    [{ms_availability_status, Ms_Availability_Status}] =
        ets:lookup(?CODE_TEMPLATES, ms_availability_status),
    Ms_Availability_Status_Length = length(Ms_Availability_Status),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Esme_Addr_Length), Esme_Addr),
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(
                            rand:uniform(Ms_Availability_Status_Length),
                            Ms_Availability_Status);
                        _ -> []
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% alert_on_msg_delivery                                             TLV 4.8.4.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(alert_on_msg_delivery = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "130C",

    Code =
%% not supported with current parser:
%%        [ParameterTag ++ "0000"] ++
    [
        lists:append([
            ParameterTag,
            "0001",
            integer_2_octet(Value)
        ])
        || Value <- lists:seq(0, 3)
    ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% billing_identification                                            TLV 4.8.4.3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(billing_identification = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "060B",
    Value = "my_billing_identification",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 1, 2),
                Tag,
                string_2_octet_string(Value)
            ])
            || Tag <- ["01", "FF"]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_receiver                                               Operation 4.1.1.3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_receiver = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{address_range, Address_Range}] =
        ets:lookup(?CODE_TEMPLATES, address_range),
    Address_Range_Length = length(Address_Range),
    [{interface_version, Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, interface_version),
    Interface_Version_Length = length(Interface_Version),
    [{password, Password}] = ets:lookup(?CODE_TEMPLATES, password),
    Password_Length = length(Password),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),
    [{system_type, System_Type}] = ets:lookup(?CODE_TEMPLATES, system_type),
    System_Type_Length = length(System_Type),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    lists:nth(rand:uniform(Password_Length), Password),
                    lists:nth(rand:uniform(System_Type_Length), System_Type),
                    lists:nth(rand:uniform(Interface_Version_Length),
                        Interface_Version),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Address_Range_Length), Address_Range)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_receiver_resp                                          Operation 4.1.1.4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_receiver_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{sc_interface_version, Sc_Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, sc_interface_version),
    Sc_Interface_Version_Length = length(Sc_Interface_Version),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(
                            rand:uniform(Sc_Interface_Version_Length),
                            Sc_Interface_Version);
                        _ -> []
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_transceiver                                            Operation 4.1.1.5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_transceiver = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{address_range, Address_Range}] =
        ets:lookup(?CODE_TEMPLATES, address_range),
    Address_Range_Length = length(Address_Range),
    [{interface_version, Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, interface_version),
    Interface_Version_Length = length(Interface_Version),
    [{password, Password}] = ets:lookup(?CODE_TEMPLATES, password),
    Password_Length = length(Password),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),
    [{system_type, System_Type}] = ets:lookup(?CODE_TEMPLATES, system_type),
    System_Type_Length = length(System_Type),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    lists:nth(rand:uniform(Password_Length), Password),
                    lists:nth(rand:uniform(System_Type_Length), System_Type),
                    lists:nth(rand:uniform(Interface_Version_Length),
                        Interface_Version),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Address_Range_Length), Address_Range)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_transceiver_resp                                       Operation 4.1.1.6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_transceiver_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{sc_interface_version, Sc_Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, sc_interface_version),
    Sc_Interface_Version_Length = length(Sc_Interface_Version),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(
                            rand:uniform(Sc_Interface_Version_Length),
                            Sc_Interface_Version);
                        _ -> []
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_transmitter                                            Operation 4.1.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_transmitter = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{address_range, Address_Range}] =
        ets:lookup(?CODE_TEMPLATES, address_range),
    Address_Range_Length = length(Address_Range),
    [{interface_version, Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, interface_version),
    Interface_Version_Length = length(Interface_Version),
    [{password, Password}] = ets:lookup(?CODE_TEMPLATES, password),
    Password_Length = length(Password),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),
    [{system_type, System_Type}] = ets:lookup(?CODE_TEMPLATES, system_type),
    System_Type_Length = length(System_Type),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    lists:nth(rand:uniform(Password_Length), Password),
                    lists:nth(rand:uniform(System_Type_Length), System_Type),
                    lists:nth(rand:uniform(Interface_Version_Length),
                        Interface_Version),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Address_Range_Length), Address_Range)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bind_transmitter_resp                                       Operation 4.1.1.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(bind_transmitter_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{sc_interface_version, Sc_Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, sc_interface_version),
    Sc_Interface_Version_Length = length(Sc_Interface_Version),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    case rand:uniform(2) rem 2 of
                        1 -> lists:nth(
                            rand:uniform(Sc_Interface_Version_Length),
                            Sc_Interface_Version);
                        _ -> []
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_area_identifier                                             4.8.4.4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_area_identifier = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0606",
    Value = "my_broadcast_area_identifier_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 6, 2),
                integer_2_octet(rand:uniform(3) - 1),
                string_2_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(failed_broadcast_area_identifier, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_area_success                                            TLV 4.8.4.5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_area_success = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0608",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 100) ++ [255]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_channel_indicator                                       TLV 4.8.4.7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_channel_indicator = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0600",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(cancel_broadcast_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_content_type                                                4.8.4.8
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_content_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0601",

    Code =
        [
            lists:append([
                ParameterTag,
                "0003",
                integer_2_octet(rand:uniform(4) - 1),
                Value
            ])
            || Value <- [
            "0000",
            "0001",
            "0002",
            "0010",
            "0011",
            "0012",
            "0013",
            "0014",
            "0015",
            "0016",
            "0017",
            "0018",
            "0019",
            "001A",
            "001B",
            "001C",
            "001D",
            "001E",
            "001F",
            "0020",
            "0021",
            "0022",
            "0023",
            "0030",
            "0031",
            "0032",
            "0033",
            "0034",
            "0035",
            "0036",
            "0037",
            "0038",
            "0039",
            "0040",
            "0041",
            "0070",
            "0071",
            "0080",
            "0081",
            "0082",
            "0083",
            "0084",
            "0085",
            "0100"
        ]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_content_type_info                                           4.8.4.6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_content_type_info = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0602",
    Value = "my_broadcast_content_type_info_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 5, 2),
                string_2_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_end_time                                                TLV 4.8.4.9
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_end_time = Rule) ->
    ?CREATE_CODE_START,
    [{absolute_time, Absolute_Time}] =
        ets:lookup(?CODE_TEMPLATES, absolute_time),
    Absolute_Time_Length = length(Absolute_Time),

    ParameterTag = "0609",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(lists:nth(N, Absolute_Time)) div 2, 2),
                lists:nth(N, Absolute_Time)
            ])
            || N <- lists:seq(1, Absolute_Time_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(query_broadcast_response_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_error_status                                           TLV 4.8.4.10
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_error_status = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0607",

    Code =
        [
            lists:append([
                ParameterTag,
                "0004",
                lists:nth(rand:uniform(?ESME_LENGTH), ?ESME)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_response_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_frequency_interval                                         4.8.4.11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_frequency_interval = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0605",

    Code =
        [
            lists:append([
                ParameterTag,
                "0003",
                lists:nth(rand:uniform(8), [
                    "00",
                    "08",
                    "09",
                    "0A",
                    "0B",
                    "0C",
                    "0D",
                    "0E"
                ]),
                integer_2_octet(N, 2)
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_message_class                                          TLV 4.8.4.12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_message_class = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0603",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 3)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_rep_num                                                    4.8.4.13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_rep_num = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0604",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                "0000"
            ])
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_request_optional_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_request_optional_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{broadcast_request_optional_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_request_optional_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_REQUEST_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_response_optional_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_response_optional_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{broadcast_response_optional_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_response_optional_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_RESPONSE_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_service_group                                              4.8.4.14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_service_group = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "060A",
    Value = "my_broadcast_service_group_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 5, 2),
                string_2_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_sm                                                Operation 4.4.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{broadcast_area_identifier, Broadcast_Area_Identifier}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_area_identifier),
    Broadcast_Area_Identifier_Length = length(Broadcast_Area_Identifier),
    [{broadcast_content_type, Broadcast_Content_Type}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_content_type),
    Broadcast_Content_Type_Length = length(Broadcast_Content_Type),
    [{broadcast_rep_num, Broadcast_Rep_Num}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_rep_num),
    Broadcast_Rep_Num_Length = length(Broadcast_Rep_Num),
    [{broadcast_frequency_interval, Broadcast_Frequency_Interval}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_frequency_interval),
    Broadcast_Frequency_Interval_Length = length(Broadcast_Frequency_Interval),
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{priority_flag, Priority_Flag}] =
        ets:lookup(?CODE_TEMPLATES, priority_flag),
    Priority_Flag_Length = length(Priority_Flag),
    [{replace_if_present_flag, Replace_If_Present_Flag}] =
        ets:lookup(?CODE_TEMPLATES, replace_if_present_flag),
    Replace_If_Present_Flag_Length = length(Replace_If_Present_Flag),
    [{schedule_delivery_time, Schedule_Delivery_Time}] =
        ets:lookup(?CODE_TEMPLATES, schedule_delivery_time),
    Schedule_Delivery_Time_Length = length(Schedule_Delivery_Time),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{sm_default_message_id, Sm_Default_Message_Id}] =
        ets:lookup(?CODE_TEMPLATES, sm_default_message_id),
    Sm_Default_Message_Id_Length = length(Sm_Default_Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{broadcast_request_optional_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_request_optional_tlvs),
    Tlvs_Length = length(Tlvs),
    [{validity_period, Validity_Period}] =
        ets:lookup(?CODE_TEMPLATES, validity_period),
    Validity_Period_Length = length(Validity_Period),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Priority_Flag_Length),
                        Priority_Flag),
                    lists:nth(rand:uniform(Schedule_Delivery_Time_Length),
                        Schedule_Delivery_Time),
                    lists:nth(rand:uniform(Validity_Period_Length),
                        Validity_Period),
                    lists:nth(rand:uniform(Replace_If_Present_Flag_Length),
                        Replace_If_Present_Flag),
                    lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                    lists:nth(rand:uniform(Sm_Default_Message_Id_Length),
                        Sm_Default_Message_Id),
                    lists:nth(rand:uniform(Broadcast_Area_Identifier_Length),
                        Broadcast_Area_Identifier),
                    lists:nth(rand:uniform(Broadcast_Content_Type_Length),
                        Broadcast_Content_Type),
                    lists:nth(rand:uniform(Broadcast_Rep_Num_Length),
                        Broadcast_Rep_Num),
                    lists:nth(rand:uniform(Broadcast_Frequency_Interval_Length),
                        Broadcast_Frequency_Interval),
                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% broadcast_sm_resp                                           Operation 4.4.1.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(broadcast_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{broadcast_response_optional_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_response_optional_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callback_num                                                     TLV 4.8.4.15
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(callback_num = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),

    ParameterTag = "0381",
    Value = "my_callback_num",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 3, 2),
                "01",
                lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                string_2_octet_string(Value)
            ])
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callback_num_atag                                                TLV 4.8.4.16
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(callback_num_atag = Rule) ->
    ?CREATE_CODE_START,
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),

    ParameterTag = "0303",
    Value = "my_callback_num_atag",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 1, 2),
                lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                string_2_octet_string(Value)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callback_num_pres_ind                                            TLV 4.8.4.17
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(callback_num_pres_ind = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0302",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 15)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cancel_broadcast_optional_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cancel_broadcast_optional_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{cancel_broadcast_optional_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, cancel_broadcast_optional_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_RESPONSE_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cancel_broadcast_sm                                         Operation 4.6.2.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cancel_broadcast_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
%% Meaning of the text in the specification is unclear (4.6.2.1 & 4.6.2.2).
%%    [{cancel_broadcast_optional_tlvs, Tlvs}] =
%%        ets:lookup(?CODE_TEMPLATES, cancel_broadcast_optional_tlvs),
%%    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr)
%% Meaning of the text in the specification is unclear (4.6.2.1 & 4.6.2.2).
%%                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
%%                        0 -> [];
%%                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
%%                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cancel_broadcast_sm_resp                                    Operation 4.6.2.3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cancel_broadcast_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cancel_sm                                                   Operation 4.5.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cancel_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Destination_Addr_Length),
                        Destination_Addr)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cancel_sm_resp                                              Operation 4.5.1.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cancel_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% command_status                                                          4.7.6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(command_status = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            15,
            17,
            255,
            256,
            274,
            1024,
            1279
        ],

    store_code(Rule, [integer_2_octet(C, 4) || C <- Code], ?MAX_BASIC, false),
    store_code(error_status_code, [integer_2_octet(C, 4) || C <- Code],
        ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_coding                                                             4.7.7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(data_coding = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            7,
            15,
            31,
            63,
            127,
            255
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_sm                                                     Operation 4.2.2.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(data_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),
    [{esm_class, Esm_Class}] = ets:lookup(?CODE_TEMPLATES, esm_class),
    Esm_Class_Length = length(Esm_Class),
    [{registered_delivery, Registered_Delivery}] =
        ets:lookup(?CODE_TEMPLATES, registered_delivery),
    Registered_Delivery_Length = length(Registered_Delivery),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{message_submission_request_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_request_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Destination_Addr_Length),
                        Destination_Addr),
                    lists:nth(rand:uniform(Esm_Class_Length), Esm_Class),
                    lists:nth(rand:uniform(Registered_Delivery_Length),
                        Registered_Delivery),
                    lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% data_sm_resp                                                Operation 4.2.2.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(data_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_submission_response_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_response_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% deliver_sm                                                  Operation 4.3.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(deliver_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),
    [{esm_class, Esm_Class}] = ets:lookup(?CODE_TEMPLATES, esm_class),
    Esm_Class_Length = length(Esm_Class),
    [{priority_flag, Priority_Flag}] =
        ets:lookup(?CODE_TEMPLATES, priority_flag),
    Priority_Flag_Length = length(Priority_Flag),
    [{protocol_id, Protocol_Id}] = ets:lookup(?CODE_TEMPLATES, protocol_id),
    Protocol_Id_Length = length(Protocol_Id),
    [{registered_delivery, Registered_Delivery}] =
        ets:lookup(?CODE_TEMPLATES, registered_delivery),
    Registered_Delivery_Length = length(Registered_Delivery),
    [{replace_if_present_flag, Replace_If_Present_Flag}] =
        ets:lookup(?CODE_TEMPLATES, replace_if_present_flag),
    Replace_If_Present_Flag_Length = length(Replace_If_Present_Flag),
    [{schedule_delivery_time, Schedule_Delivery_Time}] =
        ets:lookup(?CODE_TEMPLATES, schedule_delivery_time),
    Schedule_Delivery_Time_Length = length(Schedule_Delivery_Time),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{short_message, Short_Message}] =
        ets:lookup(?CODE_TEMPLATES, short_message),
    Short_Message_Length = length(Short_Message),
    [{sm_default_message_id, Sm_Default_Message_Id}] =
        ets:lookup(?CODE_TEMPLATES, sm_default_message_id),
    Sm_Default_Message_Id_Length = length(Sm_Default_Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{message_delivery_request_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_delivery_request_tlvs),
    Tlvs_Length = length(Tlvs),
    [{validity_period, Validity_Period}] =
        ets:lookup(?CODE_TEMPLATES, validity_period),
    Validity_Period_Length = length(Validity_Period),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Destination_Addr_Length),
                        Destination_Addr),
                    lists:nth(rand:uniform(Esm_Class_Length), Esm_Class),
                    lists:nth(rand:uniform(Protocol_Id_Length), Protocol_Id),
                    lists:nth(rand:uniform(Priority_Flag_Length),
                        Priority_Flag),
                    lists:nth(rand:uniform(Schedule_Delivery_Time_Length),
                        Schedule_Delivery_Time),
                    lists:nth(rand:uniform(Validity_Period_Length),
                        Validity_Period),
                    lists:nth(rand:uniform(Registered_Delivery_Length),
                        Registered_Delivery),
                    lists:nth(rand:uniform(Replace_If_Present_Flag_Length),
                        Replace_If_Present_Flag),
                    lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                    lists:nth(rand:uniform(Sm_Default_Message_Id_Length),
                        Sm_Default_Message_Id),
                    lists:nth(rand:uniform(Short_Message_Length),
                        Short_Message),
                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% deliver_sm_resp                                             Operation 4.3.1.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(deliver_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_delivery_response_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_delivery_response_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delivery_failure_reason                                          TLV 4.8.4.19
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(delivery_failure_reason = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0425",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 3)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_response_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_response_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),

    Code =
        [
            create_dest(rand:uniform(?MAX_DEST), Addr_Npi, Addr_Npi_Length,
                Addr_Ton, Addr_Ton_Length, Destination_Addr,
                Destination_Addr_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_addr_np_country                                             TLV 4.8.4.20
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_addr_np_country = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0613",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) div 2, 2),
                Value
            ])
            || Value <- [
            integer_2_octet(1),
            integer_2_octet(41),
            integer_2_octet(423, 2),
            integer_2_octet(44, 2),
            integer_2_octet(49, 2),
            integer_2_octet(8818, 2),
            integer_2_octet(88210, 3)
        ]],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_addr_np_information                                         TLV 4.8.4.21
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_addr_np_information = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0612",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(10, 2),
                string_2_octet_string(Value)
            ])
            || Value <- ["2025331234", "2025336789", "8001234567"]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_addr_np_resolution                                          TLV 4.8.4.22
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_addr_np_resolution = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0611",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_addr_subunit                                                TLV 4.8.4.23
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_addr_subunit = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0005",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 4)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_bearer_type                                                TLV 4.8.4.24
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_bearer_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0007",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 8)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_network_id                                                  TLV 4.8.4.25
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_network_id = Rule) ->
    ?CREATE_CODE_START,
    [{network_id, Network_Id}] = ets:lookup(?CODE_TEMPLATES, network_id),
    Network_Id_Length = length(Network_Id),

    ParameterTag = "060E",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(lists:nth(N, Network_Id)) div 2, 2),
                lists:nth(N, Network_Id)
            ])
            || N <- lists:seq(1, Network_Id_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_network_type                                                TLV 4.8.4.26
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_network_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0006",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 8)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_node_id                                                     TLV 4.8.4.27
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_node_id = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0610",

    Code =
        [
            lists:append([
                ParameterTag,
                "0006",
                Value
            ])
            || Value <- [
            "393132333435",
            "383132333435",
            "373132333435",
            "363132333435",
            "353132333435",
            "343132333435",
            "333132333435",
            "323132333435",
            "313932333435",
            "303832333435",
            "303732333435"
        ]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_port                                                        TLV 4.8.4.30
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_port = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020B",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(4294967295), 2)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_subaddress                                                  TLV 4.8.4.28
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_subaddress = Rule) ->
    ?CREATE_CODE_START,
    [{subaddress, Subaddress}] = ets:lookup(?CODE_TEMPLATES, subaddress),
    Subaddress_Length = length(Subaddress),

    ParameterTag = "0203",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(lists:nth(N, Subaddress)) div 2, 2),
                lists:nth(N, Subaddress)
            ])
            || N <- lists:seq(1, Subaddress_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest_telematics_id                                               TLV 4.8.4.29
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dest_telematics_id = Rule) ->
    ?CREATE_CODE_START,
    [{protocol_id, Protocol_Id}] = ets:lookup(?CODE_TEMPLATES, protocol_id),
    Protocol_Id_Length = length(Protocol_Id),

    ParameterTag = "0008",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                lists:nth(N, Protocol_Id),
                "00"
            ])
            || N <- lists:seq(1, Protocol_Id_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% destination_addr                                                       4.7.8
%% esme_addr                                                              4.7.11
%% source_addr                                                            4.7.29
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(destination_addr = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "127.0.0.1",
            "168.0.0.1",
            "168.123.234.321",
            "192.1.1.10",
            "192.168.1.1"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    store_code(esme_addr, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    store_code(source_addr, [string_2_c_octet_string(C) || C <- Code],
        ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% display_time                                                     TLV 4.8.4.31
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(display_time = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "1201",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dpf_result                                                       TLV 4.8.4.32
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(dpf_result = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0420",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_response_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enquire_link                                                Operation 4.1.2.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(enquire_link = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [{
            Rule,
            "00000000",
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% enquire_link_resp                                           Operation 4.1.2.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(enquire_link_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% error_code
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(error_code = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            integer_2_octet(Value)
            || Value <- lists:seq(0, 255)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% esm_class                                                              4.7.12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(esm_class = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            4,
            8,
            16,
            24,
            32,
            64,
            128,
            192
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% failed_broadcast_area_identifier                                      4.8.4.4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(failed_broadcast_area_identifier = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0606",
    Value = "my_failed_broadcast_area_identifier_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 6, 2),
                integer_2_octet(rand:uniform(3) - 1),
                string_2_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_response_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generic_nack                                                Operation 4.1.4.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(generic_nack = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [{
            Rule,
            "00000000",
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interface_version                                                      4.7.13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(interface_version = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            17,
            18,
            19,
            20,
            21,
            22,
            23,
            24,
            25,
            26,
            27,
            28,
            29,
            30,
            31,
            32,
            33,
            34,
            50
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% its_reply_type                                                   TLV 4.8.4.33
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(its_reply_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "1380",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 8)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% its_session_info                                                 TLV 4.8.4.34
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(its_session_info = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "1383",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(255)),
                integer_2_octet(rand:uniform(65535))
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% language_indicator                                               TLV 4.8.4.35
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(language_indicator = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020D",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 5)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_delivery_response_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_delivery_response_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{message_delivery_response_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, message_delivery_response_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_RESPONSE_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_delivery_request_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_delivery_request_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{message_delivery_request_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, message_delivery_request_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_REQUEST_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_id                                                             4.7.14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_id = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "this_could_be_a_message_id"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_payload                                                  TLV 4.8.4.36
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_payload = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0424",
    Value = "my_message_payload_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 5, 2),
                string_2_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_replacement_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_state                                                          4.7.15
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_state = Rule) ->

    ?CREATE_CODE_START,

    Code =
        [
            integer_2_octet(Value)
            || Value <- lists:seq(0, 9)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_state                                                    TLV 4.8.4.37
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_state_tlv = Rule) ->

    ?CREATE_CODE_START,

    ParameterTag = "0427",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 9)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_submission_response_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_submission_response_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{message_submission_response_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_response_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_RESPONSE_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% message_submission_request_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(message_submission_request_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{message_submission_request_tlv, Tlv}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_request_tlv),
    Tlv_Length = length(Tlv),

    Code =
        [
            create_tlvs(rand:uniform(
                rand:uniform(?MAX_REQUEST_TLV)), Tlv,
                Tlv_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% more_messages_to_send                                            TLV 4.8.4.38
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(more_messages_to_send = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0426",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ms_availability_status                                           TLV 4.8.4.39
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ms_availability_status = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0422",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ms_msg_wait_facilities                                           TLV 4.8.4.40
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ms_msg_wait_facilities = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0030",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- [0, 1, 2, 3, 128, 129, 130, 131]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ms_validity                                                      TLV 4.8.4.41
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ms_validity = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "1204",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 3)
        ] ++
        [
            lists:append([
                ParameterTag,
                "0004",
                "04",
                integer_2_octet(Value),
                integer_2_octet(rand:uniform(65535), 2)
            ])
            || Value <- lists:seq(0, 6)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% network_error_code                                               TLV 4.8.4.42
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(network_error_code = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0423",

    Code =
        [
            lists:append([
                ParameterTag,
                "0003",
                integer_2_octet(rand:uniform(8)),
                string_2_octet_string(lists:flatten(
                    io_lib:format("~2.2.0w", [rand:uniform(99)])))
            ])
            || _ <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_response_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_response_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% network_id
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(network_id = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            case rand:uniform(4) rem 4 of
                1 -> string_2_c_octet_string(
                    "1" ++ lists:nth(rand:uniform(?MCC_MNC_LENGTH), ?MCC_MNC));
                2 -> string_2_c_octet_string(lists:append([
                    "2",
                    lists:nth(rand:uniform(?MCC_LENGTH), ?MCC),
                    lists:flatten(
                        io_lib:format("~5.5.0w", [rand:uniform(99999)]))
                ]));
                3 -> string_2_c_octet_string(lists:append([
                    "31",
                    lists:flatten(
                        io_lib:format("~3.3.0w", [rand:uniform(255)])),
                    lists:flatten(
                        io_lib:format("~3.3.0w", [rand:uniform(255)])),
                    lists:flatten(
                        io_lib:format("~3.3.0w", [rand:uniform(255)])),
                    lists:flatten(io_lib:format("~3.3.0w", [rand:uniform(255)]))
                ]));
                _ -> string_2_c_octet_string(lists:append([
                    "32smpp.",
                    integer_to_list(N),
                    ".esme.com"
                ]))
            end
            || N <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% number_of_messages                                               TLV 4.8.4.43
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(number_of_messages = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0304",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 99)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% outbind                                                     Operation 4.1.1.7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(outbind = Rule) ->
    ?CREATE_CODE_START,
    [{password, Password}] = ets:lookup(?CODE_TEMPLATES, password),
    Password_Length = length(Password),
    [{system_id, System_Id}] = ets:lookup(?CODE_TEMPLATES, system_id),
    System_Id_Length = length(System_Id),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(System_Id_Length), System_Id),
                    lists:nth(rand:uniform(Password_Length), Password)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% password                                                               4.7.18
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(password = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "secret08"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% payload_type                                                     TLV 4.8.4.44
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(payload_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0019",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% priority_flag                                                          4.7.19
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(priority_flag = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            4
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% privacy_indicator                                                TLV 4.8.4.45
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(privacy_indicator = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0201",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 3)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% protocol_id                                                            4.7.20
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(protocol_id = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            2,
            3,
            7,
            15,
            31,
            63,
            64,
            65,
            66,
            67,
            68
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_broadcast_response_optional_tlvs
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_broadcast_response_optional_tlvs = Rule) ->
    ?CREATE_CODE_START,
    [{broadcast_end_time, Broadcast_End_Time}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_end_time),
    Broadcast_End_Time_Length = length(Broadcast_End_Time),
    [{user_message_reference, User_Message_Reference}] =
        ets:lookup(?CODE_TEMPLATES, user_message_reference),
    User_Message_Reference_Length = length(User_Message_Reference),

    Code =
        [
            case rand:uniform(4) rem 4 of
                0 -> lists:nth(rand:uniform(Broadcast_End_Time_Length),
                    Broadcast_End_Time);
                1 -> lists:nth(rand:uniform(Broadcast_End_Time_Length),
                    Broadcast_End_Time) ++
                lists:nth(rand:uniform(User_Message_Reference_Length),
                    User_Message_Reference);
                2 -> lists:nth(rand:uniform(User_Message_Reference_Length),
                    User_Message_Reference);
                _ -> lists:nth(rand:uniform(User_Message_Reference_Length),
                    User_Message_Reference) ++
                lists:nth(rand:uniform(Broadcast_End_Time_Length),
                    Broadcast_End_Time)
            end
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_broadcast_sm                                          Operation 4.6.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_broadcast_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{user_message_reference, User_Message_Reference}] =
        ets:lookup(?CODE_TEMPLATES, user_message_reference),
    User_Message_Reference_Length = length(User_Message_Reference),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    case rand:uniform(2) rem 2 of
                        0 -> [];
                        _ -> lists:nth(
                            rand:uniform(User_Message_Reference_Length),
                            User_Message_Reference)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_broadcast_sm_resp                                     Operation 4.6.1.3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_broadcast_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{broadcast_area_identifier, Broadcast_Area_Identifier}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_area_identifier),
    Broadcast_Area_Identifier_Length = length(Broadcast_Area_Identifier),
    [{broadcast_area_success, Broadcast_Area_Success}] =
        ets:lookup(?CODE_TEMPLATES, broadcast_area_success),
    Broadcast_Area_Success_Length = length(Broadcast_Area_Success),
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_state_tlv, Message_State_Tlv}] =
        ets:lookup(?CODE_TEMPLATES, message_state_tlv),
    Message_State_Tlv_Length = length(Message_State_Tlv),
    [{query_broadcast_response_optional_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, query_broadcast_response_optional_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Message_State_Tlv_Length),
                        Message_State_Tlv),
                    lists:nth(
                        rand:uniform(Broadcast_Area_Identifier_Length),
                        Broadcast_Area_Identifier),
                    lists:nth(rand:uniform(Broadcast_Area_Success_Length),
                        Broadcast_Area_Success),
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_sm                                                    Operation 4.5.2.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_sm_resp                                               Operation 4.5.2.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{error_code, Error_Code}] = ets:lookup(?CODE_TEMPLATES, error_code),
    Error_Code_Length = length(Error_Code),
    [{final_date, Final_Date}] = ets:lookup(?CODE_TEMPLATES, final_date),
    Final_Date_Length = length(Final_Date),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_state, Message_State}] =
        ets:lookup(?CODE_TEMPLATES, message_state),
    Message_State_Length = length(Message_State),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Final_Date_Length), Final_Date),
                    lists:nth(rand:uniform(Message_State_Length),
                        Message_State),
                    lists:nth(rand:uniform(Error_Code_Length), Error_Code)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% qos_time_to_live                                                 TLV 4.8.4.46
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(qos_time_to_live = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0017",

    Code =
        [
            lists:append([
                ParameterTag,
                "0004",
                integer_2_octet(rand:uniform(9999), 4)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% receipted_message_id                                             TLV 4.8.4.47
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(receipted_message_id = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "001E",
    Value = "my_receipted_message_id_",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(Value) + 6, 2),
                string_2_c_octet_string(
                    Value ++ lists:flatten(io_lib:format("~5.5.0w", [N])))
            ])
            || N <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% registered_delivery                                                    4.7.21
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(registered_delivery = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1,
            4,
            5,
            8,
            12,
            16
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% relative_time
%% -----------------------------------------------------------------------------
%% schedule_delivery_time                                               4.7.23.1
%% validity_period                                                      4.7.23.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relative_time = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "",
            "990101000000000R",
            "990201000000000R",
            "990310000000000R",
            "990427200000000R",
            "990526195600000R",
            "990625185545000R",
            "990724175444000R",
            "990823165343000R",
            "990922155242000R",
            "991021145141000R"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    store_code(schedule_delivery_time,
        [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC, false),
    store_code(validity_period, [string_2_c_octet_string(C) || C <- Code],
        ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace_if_present_flag                                                4.7.22
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(replace_if_present_flag = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            0,
            1
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace_sm                                                  Operation 4.5.3.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(replace_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_payload, Message_Payload}] =
        ets:lookup(?CODE_TEMPLATES, message_payload),
    Message_Payload_Length = length(Message_Payload),
    [{registered_delivery, Registered_Delivery}] =
        ets:lookup(?CODE_TEMPLATES, registered_delivery),
    Registered_Delivery_Length = length(Registered_Delivery),
    [{schedule_delivery_time, Schedule_Delivery_Time}] =
        ets:lookup(?CODE_TEMPLATES, schedule_delivery_time),
    Schedule_Delivery_Time_Length = length(Schedule_Delivery_Time),
    [{short_message, Short_Message}] =
        ets:lookup(?CODE_TEMPLATES, short_message),
    Short_Message_Length = length(Short_Message),
    [{sm_default_message_id, Sm_Default_Message_Id}] =
        ets:lookup(?CODE_TEMPLATES, sm_default_message_id),
    Sm_Default_Message_Id_Length = length(Sm_Default_Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{validity_period, Validity_Period}] =
        ets:lookup(?CODE_TEMPLATES, validity_period),
    Validity_Period_Length = length(Validity_Period),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Schedule_Delivery_Time_Length),
                        Schedule_Delivery_Time),
                    lists:nth(rand:uniform(Validity_Period_Length),
                        Validity_Period),
                    lists:nth(rand:uniform(Registered_Delivery_Length),
                        Registered_Delivery),
                    lists:nth(rand:uniform(Sm_Default_Message_Id_Length),
                        Sm_Default_Message_Id),
                    lists:nth(rand:uniform(Short_Message_Length),
                        Short_Message),
                    lists:nth(rand:uniform(Message_Payload_Length),
                        Message_Payload)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace_sm_resp                                             Operation 4.5.3.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(replace_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sar_msg_ref_num                                                  TLV 4.8.4.48
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sar_msg_ref_num = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020C",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(4294967295), 2)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sar_segment_seqnum                                               TLV 4.8.4.49
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sar_segment_seqnum = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020F",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(rand:uniform(255))
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sar_total_segments                                               TLV 4.8.4.50
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sar_total_segments = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020E",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(rand:uniform(255))
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sc_interface_version                                             TLV 4.8.4.51
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sc_interface_version = Rule) ->
    ?CREATE_CODE_START,
    [{interface_version, Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, interface_version),
    Interface_Version_Length = length(Interface_Version),

    ParameterTag = "0210",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                lists:nth(rand:uniform(Interface_Version_Length),
                    Interface_Version)
            ])
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% service_type                                                           4.7.25
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(service_type = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "",
            "CBS",
            "CMT",
            "CPT",
%%            not yet supported
%%            "custom",
            "GUTS",
            "USSD",
            "VMA",
            "VMN",
            "WAP"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set_dpf                                                          TLV 4.8.4.52
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(set_dpf = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0421",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 1)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% short_message                                                          4.7.26
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(short_message = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "",
            "T",
            "This",
            "This is",
            "This is a",
            "This is a short",
            "This is a short message",
            lists:append([
                "1 This is a short message"
                "2 This is a short message"
                "3 This is a short message"
                "4 This is a short message"
                "5 This is a short message"
                "6 This is a short message"
                "7 This is a short message"
                "8 This is a short message"
                "9 This is a short message"
                "A This is a short message"
                "B End"
            ])
        ],

    store_code(Rule,
        [integer_2_octet(length(C)) ++ string_2_octet_string(C) || C <- Code],
        ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sm_default_message_id                                                  4.7.27
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sm_default_message_id = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            1,
            2,
            3,
            7,
            15,
            31,
            63,
            127,
            255
        ],

    store_code(Rule, [integer_2_octet(C) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sms_signal                                                       TLV 4.8.4.53
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sms_signal = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "1203",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(4294967295), 2)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_addr_subunit                                              TLV 4.8.4.54
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_addr_subunit = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "000D",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 4)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_bearer_type                                               TLV 4.8.4.55
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_bearer_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "000F",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 8)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_network_id                                                TLV 4.8.4.56
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_network_id = Rule) ->
    ?CREATE_CODE_START,
    [{network_id, Network_Id}] = ets:lookup(?CODE_TEMPLATES, network_id),
    Network_Id_Length = length(Network_Id),

    ParameterTag = "060D",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(lists:nth(N, Network_Id)) div 2, 2),
                lists:nth(N, Network_Id)
            ])
            || N <- lists:seq(1, Network_Id_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_network_type                                              TLV 4.8.4.57
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_network_type = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "000E",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 8)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_node_id                                                   TLV 4.8.4.58
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_node_id = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "060F",

    Code =
        [
            lists:append([
                ParameterTag,
                "0006",
                Value
            ])
            || Value <- [
            "393132333435",
            "383132333435",
            "373132333435",
            "363132333435",
            "353132333435",
            "343132333435",
            "333132333435",
            "323132333435",
            "313932333435",
            "303832333435",
            "303732333435"
        ]
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_port                                                      TLV 4.8.4.59
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_port = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "020A",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(4294967295), 2)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_subaddress                                                TLV 4.8.4.60
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_subaddress = Rule) ->
    ?CREATE_CODE_START,
    [{subaddress, Subaddress}] = ets:lookup(?CODE_TEMPLATES, subaddress),
    Subaddress_Length = length(Subaddress),

    ParameterTag = "0202",

    Code =
        [
            lists:append([
                ParameterTag,
                integer_2_octet(length(lists:nth(N, Subaddress)) div 2, 2),
                lists:nth(N, Subaddress)
            ])
            || N <- lists:seq(1, Subaddress_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_telematics_id                                             TLV 4.8.4.61
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_telematics_id = Rule) ->
    ?CREATE_CODE_START,
    [{protocol_id, Protocol_Id}] = ets:lookup(?CODE_TEMPLATES, protocol_id),
    Protocol_Id_Length = length(Protocol_Id),

    ParameterTag = "0010",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                lists:nth(N, Protocol_Id),
                "00"
            ])
            || N <- lists:seq(1, Protocol_Id_Length)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% subaddress
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(subaddress = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
                "A0" ++
                string_2_octet_string("user_specified_" ++ integer_to_list(N))
            || N <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% submit_multi                                                Operation 4.2.3.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(submit_multi = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),
    [{dest, Dest}] = ets:lookup(?CODE_TEMPLATES, dest),
    Dest_Length = length(Dest),
    [{esm_class, Esm_Class}] = ets:lookup(?CODE_TEMPLATES, esm_class),
    Esm_Class_Length = length(Esm_Class),
    [{priority_flag, Priority_Flag}] =
        ets:lookup(?CODE_TEMPLATES, priority_flag),
    Priority_Flag_Length = length(Priority_Flag),
    [{protocol_id, Protocol_Id}] = ets:lookup(?CODE_TEMPLATES, protocol_id),
    Protocol_Id_Length = length(Protocol_Id),
    [{registered_delivery, Registered_Delivery}] =
        ets:lookup(?CODE_TEMPLATES, registered_delivery),
    Registered_Delivery_Length = length(Registered_Delivery),
    [{replace_if_present_flag, Replace_If_Present_Flag}] =
        ets:lookup(?CODE_TEMPLATES, replace_if_present_flag),
    Replace_If_Present_Flag_Length = length(Replace_If_Present_Flag),
    [{schedule_delivery_time, Schedule_Delivery_Time}] =
        ets:lookup(?CODE_TEMPLATES, schedule_delivery_time),
    Schedule_Delivery_Time_Length = length(Schedule_Delivery_Time),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{short_message, Short_Message}] =
        ets:lookup(?CODE_TEMPLATES, short_message),
    Short_Message_Length = length(Short_Message),
    [{sm_default_message_id, Sm_Default_Message_Id}] =
        ets:lookup(?CODE_TEMPLATES, sm_default_message_id),
    Sm_Default_Message_Id_Length = length(Sm_Default_Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{message_submission_request_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_request_tlvs),
    Tlvs_Length = length(Tlvs),
    [{validity_period, Validity_Period}] =
        ets:lookup(?CODE_TEMPLATES, validity_period),
    Validity_Period_Length = length(Validity_Period),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Dest_Length), Dest),
                    lists:nth(rand:uniform(Esm_Class_Length), Esm_Class),
                    lists:nth(rand:uniform(Protocol_Id_Length), Protocol_Id),
                    lists:nth(rand:uniform(Priority_Flag_Length),
                        Priority_Flag),
                    lists:nth(rand:uniform(Schedule_Delivery_Time_Length),
                        Schedule_Delivery_Time),
                    lists:nth(rand:uniform(Validity_Period_Length),
                        Validity_Period),
                    lists:nth(rand:uniform(Registered_Delivery_Length),
                        Registered_Delivery),
                    lists:nth(rand:uniform(Replace_If_Present_Flag_Length),
                        Replace_If_Present_Flag),
                    lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                    lists:nth(rand:uniform(Sm_Default_Message_Id_Length),
                        Sm_Default_Message_Id),
                    lists:nth(rand:uniform(Short_Message_Length),
                        Short_Message),
                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% submit_multi_resp                                           Operation 4.2.3.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(submit_multi_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_submission_response_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_response_tlvs),
    Tlvs_Length = length(Tlvs),
    [{unsuccess, Unsuccess}] = ets:lookup(?CODE_TEMPLATES, unsuccess),
    Unsuccess_Length = length(Unsuccess),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    case rand:uniform(2) rem 2 of
                        0 -> integer_2_octet(0);
                        _ ->
                            lists:nth(rand:uniform(Unsuccess_Length), Unsuccess)
                    end,
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% submit_sm                                                   Operation 4.2.1.1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(submit_sm = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{data_coding, Data_Coding}] = ets:lookup(?CODE_TEMPLATES, data_coding),
    Data_Coding_Length = length(Data_Coding),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),
    [{esm_class, Esm_Class}] = ets:lookup(?CODE_TEMPLATES, esm_class),
    Esm_Class_Length = length(Esm_Class),
    [{priority_flag, Priority_Flag}] =
        ets:lookup(?CODE_TEMPLATES, priority_flag),
    Priority_Flag_Length = length(Priority_Flag),
    [{protocol_id, Protocol_Id}] = ets:lookup(?CODE_TEMPLATES, protocol_id),
    Protocol_Id_Length = length(Protocol_Id),
    [{registered_delivery, Registered_Delivery}] =
        ets:lookup(?CODE_TEMPLATES, registered_delivery),
    Registered_Delivery_Length = length(Registered_Delivery),
    [{replace_if_present_flag, Replace_If_Present_Flag}] =
        ets:lookup(?CODE_TEMPLATES, replace_if_present_flag),
    Replace_If_Present_Flag_Length = length(Replace_If_Present_Flag),
    [{schedule_delivery_time, Schedule_Delivery_Time}] =
        ets:lookup(?CODE_TEMPLATES, schedule_delivery_time),
    Schedule_Delivery_Time_Length = length(Schedule_Delivery_Time),
    [{service_type, Service_Type}] = ets:lookup(?CODE_TEMPLATES, service_type),
    Service_Type_Length = length(Service_Type),
    [{short_message, Short_Message}] =
        ets:lookup(?CODE_TEMPLATES, short_message),
    Short_Message_Length = length(Short_Message),
    [{sm_default_message_id, Sm_Default_Message_Id}] =
        ets:lookup(?CODE_TEMPLATES, sm_default_message_id),
    Sm_Default_Message_Id_Length = length(Sm_Default_Message_Id),
    [{source_addr, Source_Addr}] = ets:lookup(?CODE_TEMPLATES, source_addr),
    Source_Addr_Length = length(Source_Addr),
    [{message_submission_request_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_request_tlvs),
    Tlvs_Length = length(Tlvs),
    [{validity_period, Validity_Period}] =
        ets:lookup(?CODE_TEMPLATES, validity_period),
    Validity_Period_Length = length(Validity_Period),

    Code =
        [{
            Rule,
            "00000000",
            lists:append(
                [
                    lists:nth(rand:uniform(Service_Type_Length), Service_Type),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Source_Addr_Length), Source_Addr),
                    lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                    lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                    lists:nth(rand:uniform(Destination_Addr_Length),
                        Destination_Addr),
                    lists:nth(rand:uniform(Esm_Class_Length), Esm_Class),
                    lists:nth(rand:uniform(Protocol_Id_Length), Protocol_Id),
                    lists:nth(rand:uniform(Priority_Flag_Length),
                        Priority_Flag),
                    lists:nth(rand:uniform(Schedule_Delivery_Time_Length),
                        Schedule_Delivery_Time),
                    lists:nth(rand:uniform(Validity_Period_Length),
                        Validity_Period),
                    lists:nth(rand:uniform(Registered_Delivery_Length),
                        Registered_Delivery),
                    lists:nth(rand:uniform(Replace_If_Present_Flag_Length),
                        Replace_If_Present_Flag),
                    lists:nth(rand:uniform(Data_Coding_Length), Data_Coding),
                    lists:nth(rand:uniform(Sm_Default_Message_Id_Length),
                        Sm_Default_Message_Id),
                    lists:nth(rand:uniform(Short_Message_Length),
                        Short_Message),
                    case rand:uniform(?MAX_REQUEST_TLV) rem ?MAX_REQUEST_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% submit_sm_resp                                              Operation 4.2.1.2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(submit_sm_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),
    [{message_id, Message_Id}] = ets:lookup(?CODE_TEMPLATES, message_id),
    Message_Id_Length = length(Message_Id),
    [{message_submission_response_tlvs, Tlvs}] =
        ets:lookup(?CODE_TEMPLATES, message_submission_response_tlvs),
    Tlvs_Length = length(Tlvs),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            lists:append(
                [
                    lists:nth(rand:uniform(Message_Id_Length), Message_Id),
                    case rand:uniform(?MAX_RESPONSE_TLV) rem
                        ?MAX_RESPONSE_TLV of
                        0 -> [];
                        _ -> lists:nth(rand:uniform(Tlvs_Length), Tlvs)
                    end
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_id                                                              4.7.30
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_id = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "SMPP3TEST"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% system_type                                                            4.7.31
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(system_type = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
            "OTA",
            "SUBMIT1",
            "VMS"
        ],

    store_code(Rule, [string_2_c_octet_string(C) || C <- Code], ?MAX_BASIC,
        false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unbind                                                      Operation 4.1.1.8
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unbind = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [{
            Rule,
            "00000000",
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unbind_resp                                                 Operation 4.1.1.9
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unbind_resp = Rule) ->
    ?CREATE_CODE_START,
    [{command_status, Command_Status}] =
        ets:lookup(?CODE_TEMPLATES, command_status),
    Command_Status_Length = length(Command_Status),

    Code =
        [{
            Rule,
            lists:nth(rand:uniform(Command_Status_Length), Command_Status),
            []}
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unsuccess
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unsuccess = Rule) ->
    ?CREATE_CODE_START,
    [{addr_npi, Addr_Npi}] = ets:lookup(?CODE_TEMPLATES, addr_npi),
    Addr_Npi_Length = length(Addr_Npi),
    [{addr_ton, Addr_Ton}] = ets:lookup(?CODE_TEMPLATES, addr_ton),
    Addr_Ton_Length = length(Addr_Ton),
    [{destination_addr, Destination_Addr}] =
        ets:lookup(?CODE_TEMPLATES, destination_addr),
    Destination_Addr_Length = length(Destination_Addr),
    [{error_status_code, Error_Status_Code}] =
        ets:lookup(?CODE_TEMPLATES, error_status_code),
    Error_Status_Code_Length = length(Error_Status_Code),

    Code =
        [
            create_unsuccess(rand:uniform(?MAX_UNSUCCESS), Addr_Npi,
                Addr_Npi_Length, Addr_Ton, Addr_Ton_Length, Destination_Addr,
                Destination_Addr_Length, Error_Status_Code,
                Error_Status_Code_Length)
            || _ <- lists:seq(1, ?MAX_BASIC * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user_message_reference                                           TLV 4.8.4.62
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(user_message_reference = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0204",

    Code =
        [
            lists:append([
                ParameterTag,
                "0002",
                integer_2_octet(rand:uniform(4294967295), 2)
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(cancel_broadcast_optional_tlv, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    store_code(query_broadcast_request_optional_tlv, Code, ?MAX_TLV, false),
    store_code(query_broadcast_response_optional_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% user_response_code                                               TLV 4.8.4.63
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(user_response_code = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0205",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(rand:uniform(255))
            ])
            || _ <- lists:seq(1, ?MAX_TLV * 2)
        ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    store_code(query_broadcast_request_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ussd_service_op                                                  TLV 4.8.4.64
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ussd_service_op = Rule) ->
    ?CREATE_CODE_START,

    ParameterTag = "0501",

    Code =
        [
            lists:append([
                ParameterTag,
                "0001",
                integer_2_octet(Value)
            ])
            || Value <- lists:seq(0, 3)
        ] ++ [
        lists:append([
            ParameterTag,
            "0001",
            integer_2_octet(Value)
        ])
        || Value <- lists:seq(16, 19)
    ],

    store_code(Rule, Code, ?MAX_TLV, false),
    store_code(message_delivery_request_tlv, Code, ?MAX_TLV, false),
    store_code(message_submission_request_tlv, Code, ?MAX_TLV, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dest
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dest(Number, Addr_Npi, Addr_Npi_Length, Addr_Ton, Addr_Ton_Length,
    Destination_Addr, Destination_Addr_Length) ->
    DestList = [
        case rand:uniform(2) rem 2 of
            1 -> lists:append([
                integer_2_octet(1),
                lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
                lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
                lists:nth(rand:uniform(Destination_Addr_Length),
                    Destination_Addr)
            ]);
            _ -> lists:append([
                integer_2_octet(2),
                string_2_c_octet_string(
                    "distribution_lst_#" ++ integer_to_list(N))
            ])
        end
        || N <- lists:seq(1, Number)
    ],
    DestUnique = sets:to_list(sets:from_list(DestList)),
    integer_2_octet(length(DestUnique)) ++ lists:flatten(DestUnique).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create operation.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_operation(Rule, _CommandStatus, PDUBody) ->
    PDU = lists:append(
        [
            integer_2_octet(length(PDUBody) div 2 + 16, 4),
            integer_2_octet(?COMMAND_ID(Rule), 4),
%% not used because of issue #25 (https://github.com/K2InformaticsGmbH/smpp_parser/issues/25)
%%            CommandStatus,
            "00000000",
            "00000001",
            PDUBody
        ]),

    ?assertEqual(0, length(PDU) rem 2, "PDU=" ++ PDU),
    create_byte_string(PDU, []).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tlv
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tlvs(Number, Tlv, Tlv_Length) ->
    TlvList = [
        lists:nth(rand:uniform(Tlv_Length), Tlv)
        || _ <- lists:seq(1, Number)
    ],
    TlvMap = maps:from_list(
        [{string:slice(T, 0, 2), string:slice(T, 2)} || T <- TlvList]),
    lists:flatten([Key ++ Value || {Key, Value} <- maps:to_list(TlvMap)]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unsuccess
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_unsuccess(Number, Addr_Npi, Addr_Npi_Length, Addr_Ton, Addr_Ton_Length,
    Destination_Addr, Destination_Addr_Length,
    Error_Status_Code, Error_Status_Code_Length) ->
    UnsuccessList = [
        lists:append([
            lists:nth(rand:uniform(Addr_Ton_Length), Addr_Ton),
            lists:nth(rand:uniform(Addr_Npi_Length), Addr_Npi),
            lists:nth(rand:uniform(Destination_Addr_Length),
                Destination_Addr),
            lists:nth(rand:uniform(Error_Status_Code_Length),
                Error_Status_Code)
        ])
        || _ <- lists:seq(1, Number)
    ],
    UnsuccessUnique = sets:to_list(sets:from_list(UnsuccessList)),
    integer_2_octet(length(UnsuccessUnique)) ++ lists:flatten(UnsuccessUnique).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_CompactedDetailed, []) ->
    ok;
file_create_ct_all(CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(CompactedDetailed, Rule, ?FUNCTIONS),
    file_create_ct_all(CompactedDetailed, Rules).

file_create_ct(_CompactedDetailed, _Rule, []) ->
    ok;
file_create_ct(CompactedDetailed, Rule, [Function | Tail]) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_CT),

    FileName = lists:append([
        CompactedDetailed,
        "_",
        RuleString,
        "_",
        atom_to_list(Function),
        "_SUITE"
    ]),
    {ok, File, _} = file:path_open([?PATH_CT], FileName ++ ".erl", [write]),

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : final common tests ===> ~12.. B file_name: ~s ",
        [CodeLength, FileName ++ ".erl"])),

    {{Current_Year, Current_Month, Current_Day}, _} = calendar:local_time(),

    io:format(File, "~s~n",
        ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n",
        [lists:append(["%%% File        : ", FileName, ".erl"])]),
    io:format(File, "~s~n", [lists:append(
        ["%%% Description : Test Suite for command: ", RuleString, " and function: ", atom_to_list(
            Function), "."])]),
    io:format(File, "~s~n", ["%%%"]),
    io:format(File, "~s~n", ["%%% Created     : " ++ lists:flatten(
        io_lib:format("~2..0w.~2..0w.~4..0w",
            [Current_Day, Current_Month, Current_Year]))]),
    io:format(File, "~s~n",
        ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["-module(", FileName, ")."])]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-export(["]),
    io:format(File, "~s~n", ["    all/0,"]),
    io:format(File, "~s~n", ["    end_per_suite/1,"]),
    io:format(File, "~s~n", ["    init_per_suite/1,"]),

    case CodeLength of
        0 -> io:format(File, "~s~n", ["    suite/0"]);
        _ -> io:format(File, "~s~n", ["    suite/0,"]),
            case CompactedDetailed of
                "compacted" ->
                    io:format(File, "~s~n",
                        [lists:append(["    test_compacted/1"])]);
                _ -> file_write_ct_export(1, File, CodeLength, Function)
            end
    end,

    io:format(File, "~s~n", ["])."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-include_lib(\"common_test/include/ct.hrl\")."]),
    io:format(File, "~s~n", ["-include_lib(\"eunit/include/eunit.hrl\")."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - SUITE"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["suite() ->"]),
    io:format(File, "~s~n", ["    ["]),
    io:format(File, "~s~n", [lists:append(
        ["        {timetrap, {minutes, ", integer_to_list(
            ?TIMETRAP_MINUTES), "}}"])]),
    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["init_per_suite(Config) ->"]),
    io:format(File, "~s~n", ["    Config."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["end_per_suite(_Config) ->"]),
    io:format(File, "~s~n", ["    ok."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - ALL"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["all() ->"]),
    io:format(File, "~s~n", ["    ["]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n",
                         [lists:append(["        test_compacted"])]);
                 _ -> file_write_ct_all(1, File, CodeLength, Function)
             end
    end,

    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% TEST CASES"]),
    io:format(File, "~s~n",
        ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),

    case CodeLength of
        0 -> ok;
        _ -> case CompactedDetailed of
                 "compacted" ->
                     io:format(File, "~s~n",
                         [lists:append(["test_compacted(_Config) ->"])]);
                 _ -> ok
             end,
            file_write_ct(1, CompactedDetailed, File, Function, Code)
    end,
    file_create_ct(CompactedDetailed, Rule, Tail).

file_write_ct(_Current, CompactedDetailed, File, _Function, []) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", ["    ok."]);
        _ -> ok
    end,
    file:close(File);
file_write_ct(Current, CompactedDetailed, File, Function, [{Command, CommandStatus, PDUBody} | T]) ->
    ok = file_write_ct_test(Current, CompactedDetailed, File,
        {Command, CommandStatus, PDUBody}, Function),
    file_write_ct(Current + 1, CompactedDetailed, File, Function, T).

file_write_ct_all(Current, File, Target, Function)
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(
        ["        test_", integer_to_list(Current), "_", atom_to_list(
            Function)])]);
file_write_ct_all(Current, File, Target, Function) ->
    io:format(File, "~s~n", [lists:append(
        ["        test_", integer_to_list(Current), "_", atom_to_list(
            Function), ","])]),
    file_write_ct_all(Current + 1, File, Target, Function).

file_write_ct_export(Current, File, Target, Function)
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(
        ["    test_", integer_to_list(Current), "_", atom_to_list(
            Function), "/1"])]);
file_write_ct_export(Current, File, Target, Function) ->
    io:format(File, "~s~n", [lists:append(
        ["    test_", integer_to_list(Current), "_", atom_to_list(
            Function), "/1,"])]),
    file_write_ct_export(Current + 1, File, Target, Function).

file_write_ct_test(Current, CompactedDetailed, File, {Command, CommandStatus, PDUBody}, Function) ->
    PDU = lists:append([
        "{",
        atom_to_list(Command),
        ", \"",
        create_operation(Command, CommandStatus, PDUBody),
        "\"}"
    ]),
    case CompactedDetailed of
        "compacted" ->
            case Current rem ?ALIVE_COUNTER of
                0 ->
                    io:format(File, "~s~n", [
                            "    io:format(user, \"Hi Travis CI, I'm still alive - next test is number " ++
                            integer_to_list(Current) ++ " :))~n\", []),"]);
                _ -> []
            end,
            io:format(File, "~s~n", [lists:append([
                "    smpp_test_utils:",
                atom_to_list(Function),
                "(",
                PDU,
                "),"
            ])]);
        _ ->
            io:format(File, "~s~n", [lists:append(
                ["test_", integer_to_list(Current), "_", atom_to_list(
                    Function), "(_Config) ->"])]),
            io:format(File, "~s~n", [lists:append([
                "    smpp_test_utils:",
                atom_to_list(Function),
                "(",
                PDU,
                ")."
            ])]),
            io:format(File, "~s~n", [""])
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert an integer into a number of octets (decimal).
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer_2_octet(Integer) ->
    integer_2_octet(Integer, 1).

integer_2_octet(Integer, NumberOctets) ->
    String = lists:flatten(lists:duplicate(NumberOctets, "00")) ++
        integer_to_list(Integer, 16),
    RT = string:slice(String, length(String) - (NumberOctets * 2)),
    ?assertEqual(0, length(RT) rem 2, "Integer=" ++ integer_to_list(Integer) ++
        " NumberOctets=" ++ integer_to_list(NumberOctets) ++ " RT=" ++ RT),
    RT.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Strict) ->
    ?D("Start~n Rule: ~p~n Code: ~p~n Max: ~p~n Strict: ~p~n",
        [Rule, Code, Max, Strict]),
    ok = check_code(Rule, Code),
    case ?LOGGING of
        true ->
            erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                " : store Code         ===> ~12.. B rule: ~s~n",
                [length(Code), atom_to_list(Rule)]));
        _ -> ok
    end,
    case Max == 0 of
        true ->
            case ?LOGGING of
                true ->
                    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : store CodeNew      ===> ~12.. B rule: ~s~n",
                        [0, atom_to_list(Rule)]));
                _ -> ok
            end;
        _ ->
            CodeUnique = ordsets:to_list(ordsets:from_list(Code)),
            CodeUnique_Length = length(CodeUnique),
            CodeUniqueSorted = lists:sort(?F_RANDOM, CodeUnique),
            CodeUniqueLimited = case CodeUnique_Length > Max of
                                    true ->
                                        lists:sublist(CodeUniqueSorted, 1, Max);
                                    _ -> CodeUnique
                                end,
            CodeTotal = case ets:lookup(?CODE_TEMPLATES, Rule) of
                            [{Rule, CodeOld}] ->
                                lists:sort(?F_RANDOM, ordsets:to_list(
                                    ordsets:from_list(lists:append(
                                        [CodeOld, CodeUniqueLimited]))));
                            _ -> CodeUniqueLimited
                        end,
            CodeTotal_Length = length(CodeTotal),
            CodeNew = case Strict andalso CodeTotal_Length > Max of
                          true ->
                              [lists:nth(rand:uniform(CodeTotal_Length),
                                  CodeTotal) || _ <- lists:seq(1, Max)];
                          _ -> CodeTotal
                      end,
            ets:insert(?CODE_TEMPLATES, {Rule, CodeNew}),
            case ?LOGGING of
                true ->
                    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
                        " : store CodeNew      ===> ~12.. B rule: ~s~n",
                        [length(CodeNew), atom_to_list(Rule)]));
                _ -> ok
            end
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert a string into a C-Octet string.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_2_c_octet_string(String) ->
    RT = string_2_octet_string(String) ++ "00",
    ?assertEqual(0, length(RT) rem 2, "String=" ++ String ++ " RT=" ++ RT),
    RT.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert a string into an Octet string.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_2_octet_string(String) ->
    RT = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- String]),
    ?assertEqual(0, length(RT) rem 2, "String=" ++ String ++ " RT=" ++ RT),
    RT.
