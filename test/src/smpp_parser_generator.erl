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
    integer_to_octet/2,
    generate/0
]).

-define(ALL_OPERATION, [
    alert_notification,
    bind_receiver,
    bind_receiver_resp,
    bind_transceiver,
    bind_transceiver_resp,
    bind_transmitter,
    bind_transmitter_resp,
    enquire_link,
    enquire_link_resp,
    outbind,
    unbind,
    unbind_resp
]).

-define(NODEBUG, true).

-include("smpp_parser_generator.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
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
        true -> ok = file_create_ct_all("compacted", ?ALL_OPERATION);
        _ -> ok = file_create_ct_all("detailed_", ?ALL_OPERATION)
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

    create_code(addr_npi),
    create_code(addr_ton),
    create_code(address_range),
    create_code(command_status),
    create_code(esme_addr),
    create_code(interface_version),
    create_code(ms_availability_status),
    create_code(password),
    create_code(source_addr),
    create_code(system_id),
    create_code(system_type),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 02
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    erlang:display(io:format(user, "~n" ++ ?MODULE_STRING ++
        " : ======================================================> create_code: Level 02   <===================~n",
        [])),

    create_code(sc_interface_version),

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
    create_code(enquire_link),
    create_code(enquire_link_resp),
    create_code(outbind),
    create_code(unbind),
    create_code(unbind_resp),

    ok.

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

    store_code(Rule, [integer_to_octet(C, 1) || C <- Code], ?MAX_BASIC, false),
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

    store_code(Rule, [integer_to_octet(C, 1) || C <- Code], ?MAX_BASIC, false),
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
                    lists:nth(rand:uniform(Ms_Availability_Status_Length),
                        Ms_Availability_Status)
                ])}
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_OPERATION, false),
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

    store_code(Rule, [integer_to_octet(C, 4) || C <- Code], ?MAX_BASIC, false),
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
%% esme_addr                                                              4.7.11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(esme_addr = Rule) ->
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

    store_code(Rule, [integer_to_octet(C, 1) || C <- Code], ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ms_availability_status                                           TLV 4.8.4.39
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(ms_availability_status = Rule) ->
    ?CREATE_CODE_START,

    Code =
        [
                "04220001" ++
                integer_to_octet(I, 1)
            || I <- [0, 1, 2]
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
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
%% sc_interface_version                                             TLV 4.8.4.51
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sc_interface_version = Rule) ->
    ?CREATE_CODE_START,
    [{interface_version, Interface_Version}] =
        ets:lookup(?CODE_TEMPLATES, interface_version),
    Interface_Version_Length = length(Interface_Version),

    Code =
        [
                "02100001" ++
                lists:nth(rand:uniform(Interface_Version_Length),
                    Interface_Version)
            || _ <- lists:seq(1, ?MAX_OPERATION * 2)
        ],

    store_code(Rule, Code, ?MAX_BASIC, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% source_addr                                                            4.7.29
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(source_addr = Rule) ->
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
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create operation.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_operation(Rule, CommandStatus, PDUBody) ->
    ?assertEqual(0, length(PDUBody) rem 2, "PDUBody=" ++ PDUBody),
    PDU = lists:append(
        [
            integer_to_octet(length(PDUBody) div 2 + 16, 4),
            integer_to_octet(?COMMAND_ID(Rule), 4),
            CommandStatus,
            integer_to_octet(rand:uniform(4294967296), 4),
            PDUBody
        ]),

    ?assertEqual(0, length(PDU) rem 2, "PDU=" ++ PDU),
    create_byte_string(PDU, []).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_CompactedDetailed, []) ->
    ok;
file_create_ct_all(CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(CompactedDetailed, Rule),
    file_create_ct_all(CompactedDetailed, Rules).

file_create_ct(CompactedDetailed, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    filelib:ensure_dir(?PATH_CT),

    FileName = lists:append([
        CompactedDetailed,
        "_",
        RuleString,
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
        ["%%% Description : Test Suite for command: ", RuleString, "."])]),
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
                _ -> file_write_ct_export(1, File, CodeLength)
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
                 _ -> file_write_ct_all(1, File, CodeLength)
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
            file_write_ct(1, CompactedDetailed, File, Code)
    end.

file_write_ct(_Current, CompactedDetailed, File, []) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", ["    ok."]);
        _ -> ok
    end,
    file:close(File);
file_write_ct(Current, CompactedDetailed, File, [{Command, CommandStatus, PDUBody} | T]) ->
    ok =
        file_write_ct_test(Current, CompactedDetailed, File,
            {Command, CommandStatus, PDUBody},
            ?FUNCTIONS),
    file_write_ct(Current + 1, CompactedDetailed, File, T).

file_write_ct_all(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n",
        [lists:append(["        test_", integer_to_list(Current)])]);
file_write_ct_all(Current, File, Target) ->
    io:format(File, "~s~n",
        [lists:append(["        test_", integer_to_list(Current), ","])]),
    file_write_ct_all(Current + 1, File, Target).

file_write_ct_export(Current, File, Target)
    when Current == Target ->
    io:format(File, "~s~n",
        [lists:append(["    test_", integer_to_list(Current), "/1"])]);
file_write_ct_export(Current, File, Target) ->
    io:format(File, "~s~n",
        [lists:append(["    test_", integer_to_list(Current), "/1,"])]),
    file_write_ct_export(Current + 1, File, Target).

file_write_ct_test(_Current, _CompactedDetailed, _File, _, []) ->
    ok;
file_write_ct_test(Current, CompactedDetailed, File, {Command, CommandStatus, PDUBody}, [Method | Tail]) ->
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
                atom_to_list(Method),
                "(",
                PDU,
                "),"
            ])]);
        _ ->
            io:format(File, "~s~n", [lists:append(
                ["test_", integer_to_list(Current), "(_Config) ->"])]),
            io:format(File, "~s~n", [lists:append([
                "    smpp_test_utils:",
                atom_to_list(Method),
                "(",
                PDU,
                ")."
            ])]),
            io:format(File, "~s~n", [""])
    end,
    file_write_ct_test(Current, CompactedDetailed, File,
        {Command, CommandStatus, PDUBody}, Tail).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert an integer into a number of octets.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer_to_octet(Integer, NumberOctets) ->
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
    ?assertEqual(length(String) * 2 + 2, length(RT),
        "String=" ++ String ++ " RT=" ++ RT),
    RT.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert a string into an Octet string.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_2_octet_string(String) ->
    RT = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- String]),
    ?assertEqual(length(String) * 2, length(RT),
        "String=" ++ String ++ " RT=" ++ RT),
    RT.
