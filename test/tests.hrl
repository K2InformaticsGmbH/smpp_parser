-ifndef(_TESTS_HRL_).
-define(_TESTS_HRL_, true).

-define(PDU(_Id,_Extra), <<"{\"command_id\":",(integer_to_binary(_Id))/binary,
                           ",\"command_status\":0,\"sequence_number\":0",
                           _Extra,"}">>).
-define(PDU(_Id), ?PDU(_Id, "")).
-define(PDU_SYSID(_Id), ?PDU(_Id, ",\"system_id\":\"\",\"sc_interface_version\":80")).
-define(PDU_DSTADDR(_Id), ?PDU(_Id, ",\"destination_addr\":\"\"")).

-define(TESTS,
[{"bind_transceiver",
  "00 00 00 23 00 00 00 09 00 00 00 00 00 00 00 01 34 31 31 35 00 7A 75 6A 5F "
  "34 31 31 35 00 00 34 00 00 00",
  #{addr_npi => <<"Unknown">>,addr_ton => <<"Unknown">>, address_range => <<>>,
    command_id => <<"bind_transceiver">>, command_length => 35,
    command_status => <<"ESME_ROK">>, interface_version => 52,
    password => <<"zuj_4115">>, sequence_number => 1,system_id => <<"4115">>,
    system_type => <<>>}},
 {"bind_transmitter",
  "00 00 00 27 00 00 00 02 00 00 00 00 00 00 00 01 34 31 31 35 00 66 64 73 66 "
  "61 6A 66 6B 00 74 65 73 74 00 34 04 08 00",
  #{addr_npi => <<"National">>, addr_ton => <<"Subscriber Number">>,
    address_range => <<>>, command_id => <<"bind_transmitter">>,
    command_length => 39,command_status => <<"ESME_ROK">>,
    interface_version => 52,password => <<"fdsfajfk">>, sequence_number => 1,
    system_id => <<"4115">>, system_type => <<"test">>}},
 {"bind_transmitter_resp",
  "00 00 00 1F 80 00 00 02 00 00 00 00 00 00 00 01 53 4D 50 50 33 54 45 53 54 "
  "00 02 10 00 01 19",
  #{command_id => <<"bind_transmitter_resp">>, command_length => 31,
    command_status => <<"ESME_ROK">>, sequence_number => 1,
    sc_interface_version => 25, system_id => <<"SMPP3TEST">>}},
 {"bind_receiver",
  "00 00 00 2A 00 00 00 01 00 00 00 00 00 00 00 01 34 31 31 35 00 66 64 67 61 "
  "73 72 67 73 00 74 65 73 74 69 6E 67 00 34 02 09 00",
  #{addr_npi => <<"Private">>,addr_ton => <<"National">>, address_range => <<>>,
    command_id => <<"bind_receiver">>, command_length => 42,
    command_status => <<"ESME_ROK">>, interface_version => 52,
    password => <<"fdgasrgs">>, sequence_number => 1, system_id => <<"4115">>,
    system_type => <<"testing">>}},
 {"submit_sm",
  "00 00 00 44 00 00 00 04 00 00 00 00 00 00 00 01 00 05 00 34 31 30 33 37 00 "
  "00 00 30 37 39 34 36 35 30 31 31 35 00 00 00 00 00 00 11 00 00 00 14 74 65 "
  "73 74 20 73 77 69 73 73 63 6F 6D 20 28 53 4D 50 50 29",
  #{command_id => <<"submit_sm">>,command_length => 68,
    command_status => <<"ESME_ROK">>, data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"Unknown">>, dest_addr_ton => <<"Unknown">>,
    destination_addr => <<"0794650115">>,esm_class => 0, priority_flag => 0,
    protocol_id => 0, registered_delivery => 17, replace_if_present_flag => 0,
    schedule_delivery_time => <<>>, sequence_number => 1,service_type => <<>>,
    short_message => <<"test swisscom (SMPP)">>, sm_default_msg_id => 0,
    source_addr => <<"41037">>, source_addr_npi => <<"Unknown">>,
    source_addr_ton => <<"Alphanumeric">>, validity_period => <<>>}},
 {"deliver_sm",
  "00 00 00 44 00 00 00 05 00 00 00 00 00 00 00 01 00 00 00 34 31 37 39 34 34 "
  "34 34 34 34 34 00 00 00 31 32 33 34 35 00 00 00 00 00 00 00 00 00 00 13 68 "
  "69 20 67 6F 74 20 79 6F 75 72 20 6D 65 73 73 61 67 65",
  #{command_id => <<"deliver_sm">>,command_length => 68,
    command_status => <<"ESME_ROK">>, data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"Unknown">>,dest_addr_ton => <<"Unknown">>,
    destination_addr => <<"12345">>,esm_class => 0, priority_flag => 0,
    protocol_id => 0,registered_delivery => 0, replace_if_present_flag => 0,
    schedule_delivery_time => <<>>, sequence_number => 1, service_type => <<>>,
    short_message => <<"hi got your message">>, sm_default_msg_id => 0,
    source_addr => <<"41794444444">>, source_addr_npi => <<"Unknown">>,
    source_addr_ton => <<"Unknown">>,validity_period => <<>>}},
 {"deliver_sm_resp",
  "00 00 00 11 80 00 00 05 00 00 00 00 00 00 00 01 00",
  #{command_id => <<"deliver_sm_resp">>,command_length => 17,
    command_status => <<"ESME_ROK">>, message_id => <<>>,
    sequence_number => 1}},
 {"submit_sm_optional",
  "00 00 00 51 00 00 00 04 00 00 00 00 00 00 00 01 34 31 31 35 00 01 09 31 39 "
  "32 2E 32 35 34 2E 32 35 34 2E 31 37 00 03 01 31 34 38 2E 32 34 37 2E 31 35 "
  "37 2E 32 35 00 03 00 00 00 00 00 00 00 00 00 02 05 00 01 04 02 04 00 02 00 "
  "03 04 26 00 01 01",
  #{command_id => <<"submit_sm">>, command_length => 81,
    command_status => <<"ESME_ROK">>, data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"ISDN (E163/E164)">>,
    dest_addr_ton => <<"Network Specific">>,
    destination_addr => <<"148.247.157.25">>, esm_class => 3,
    more_messages_to_send => 1, priority_flag => 0, protocol_id => 0,
    registered_delivery => 0, replace_if_present_flag => 0,
    schedule_delivery_time => <<>>, sequence_number => 1,
    service_type => <<"4115">>, short_message => <<>>, sm_default_msg_id => 0,
    source_addr => <<"192.254.254.17">>, source_addr_npi => <<"Private">>,
    source_addr_ton => <<"International">>, user_message_reference => 3,
    user_response_code => 4, validity_period => <<>>}},
 {"data_sm",
  "00 00 00 3D 00 00 01 03 00 00 00 00 00 00 00 01 34 31 31 35 00 03 08 31 32 "
  "35 2E 31 32 35 2E 31 32 34 2E 32 34 35 00 01 01 31 32 34 2E 31 34 37 35 2E "
  "32 35 30 2E 31 34 37 00 03 00 00",
  #{command_id => <<"data_sm">>,command_length => 61,
    command_status => <<"ESME_ROK">>,data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"ISDN (E163/E164)">>,
    dest_addr_ton => <<"International">>,
    destination_addr => <<"124.1475.250.147">>, esm_class => 3,
    registered_delivery => 0, sequence_number => 1, service_type => <<"4115">>,
    source_addr => <<"125.125.124.245">>, source_addr_npi => <<"National">>,
    source_addr_ton => <<"Network Specific">>}},
 {"query_sm",
  "00 00 00 24 00 00 00 03 00 00 00 00 00 00 00 01 35 00 01 0E 31 35 34 2E 31 "
  "35 36 2E 31 35 34 2E 31 32 34 00",
  #{command_id => <<"query_sm">>,command_length => 36,
    command_status => <<"ESME_ROK">>,message_id => <<"5">>,
    sequence_number => 1,source_addr => <<"154.156.154.124">>,
    source_addr_npi => <<"Internet (IP)">>,
    source_addr_ton => <<"International">>}},
 {"replace_sm",
  "00 00 00 52 00 00 00 07 00 00 00 00 00 00 00 01 35 00 03 0E 31 32 2E 31 35 "
  "33 2E 32 35 2E 34 38 00 31 36 30 31 30 34 31 35 34 34 32 33 36 31 32 2D 00 "
  "31 36 30 31 30 34 31 35 34 34 32 39 36 31 32 2D 00 1A 06 0C 74 65 73 74 20 "
  "6D 65 73 73 61 67 65",
  #{command_id => <<"replace_sm">>,command_length => 82,
    command_status => <<"ESME_ROK">>, message_id => <<"5">>,
    registered_delivery => 26, schedule_delivery_time => <<"160104154423612-">>,
    sequence_number => 1, short_message => <<"test message">>,
    sm_default_msg_id => 6,source_addr => <<"12.153.25.48">>,
    source_addr_npi => <<"Internet (IP)">>,
    source_addr_ton => <<"Network Specific">>,
    validity_period => <<"160104154429612-">>}},
 {"cancel_sm",
  "00 00 00 32 00 00 00 08 00 00 00 00 00 00 00 01 00 00 01 08 31 34 32 2E 32 "
  "35 2E 39 35 2E 36 38 00 01 0E 31 35 32 2E 31 34 32 2E 31 33 36 2E 37 38 00",
  #{command_id => <<"cancel_sm">>, command_length => 50,
    command_status => <<"ESME_ROK">>, dest_addr_npi => <<"Internet (IP)">>,
    dest_addr_ton => <<"International">>,
    destination_addr => <<"152.142.136.78">>, message_id => <<>>,
    sequence_number => 1, service_type => <<>>,
    source_addr => <<"142.25.95.68">>, source_addr_npi => <<"National">>,
    source_addr_ton => <<"International">>}},
  {"data_sm_resp_dpf_result",
   "00 00 00 30 80 00 01 03 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "  
   "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 04 20 00 01 00",
   #{command_id => <<"data_sm_resp">>, command_length => 48,
     command_status => <<"ESME_ROK">>,dpf_result => 0,
     message_id => <<"this_could_be_a_message_id">>, sequence_number => 1}},
  {"alert_notification_ms_availability_status",
   "00 00 00 33 00 00 01 02 00 00 00 00 00 00 00 01 02 04 31 32 37 2E 30 2E 30 "
   "2E 31 00 02 0A 31 36 38 2E 31 32 33 2E 32 33 34 2E 33 32 31 00 04 22 00 01 "
   "00",
   #{command_id => <<"alert_notification">>, command_length => 51,
     command_status => <<"ESME_ROK">>, esme_addr => <<"168.123.234.321">>,
     esme_addr_npi => <<"ERMES">>, esme_addr_ton => <<"National">>,
     ms_availability_status => 0, sequence_number => 1,
     source_addr => <<"127.0.0.1">>, source_addr_npi => <<"Telex (F.69)">>,
     source_addr_ton => <<"National">>}},
  {"submit_sm_resp_additional_status_info_text",
   "00 00 00 53 80 00 00 04 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "  
   "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D 79 5F "  
   "61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F 5F 74 65 "  
   "78 74 00 04 25 00 01 01",
   #{additional_status_info_text => <<"my_additional_status_info_text">>,
     command_id => <<"submit_sm_resp">>, command_length => 83,
     command_status => <<"ESME_ROK">>,delivery_failure_reason => 1,
     message_id => <<"this_could_be_a_message_id">>, sequence_number => 1}},
  {"submit_multi_resp_delivery_failure_reason",
   "00 00 00 36 80 00 00 21 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
   "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 04 25 00 01 02 04 "
   "20 00 01 01",
   #{command_id => <<"submit_multi_resp">>, command_length => 54,
     command_status => <<"ESME_ROK">>, delivery_failure_reason => 2,
     dpf_result => 1, message_id => <<"this_could_be_a_message_id">>,
     sequence_number => 1, unsuccess_sme => <<>>}},
  {"data_sm_dest_addr_np_country",
   "00 00 00 3C 00 00 01 03 00 00 00 00 00 00 00 01 43 4D 54 00 05 04 31 39 32 "
   "2E 31 36 38 2E 31 2E 31 00 03 0A 31 39 32 2E 31 36 38 2E 31 2E 31 00 C0 00 "
   "7F 06 13 00 05 00 00 00 22 72",
   #{command_id => <<"data_sm">>, command_length => 60,
     command_status => <<"ESME_ROK">>,data_coding => <<"127">>,
     dest_addr_np_country => 8818,dest_addr_npi => <<"ERMES">>,
     dest_addr_ton => <<"Network Specific">>,
     destination_addr => <<"192.168.1.1">>, esm_class => 192,
     registered_delivery => 0, sequence_number => 1, service_type => <<"CMT">>,
     source_addr => <<"192.168.1.1">>, source_addr_npi => <<"Telex (F.69)">>,
     source_addr_ton => <<"Alphanumeric">>}},
  {"data_sm_resp_additional_status_info_text",
   "00 00 00 6B 80 00 01 03 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
   "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D 79 5F "
   "61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F 5F 74 65 "
   "78 74 00 04 24 00 19 6D 79 5F 6D 65 73 73 61 67 65 5F 70 61 79 6C 6F 61 64 "
   "5F 30 30 30 30 39 00",
    #{additional_status_info_text => <<"my_additional_status_info_text">>,
      command_id => <<"data_sm_resp">>, command_length => 107,
      command_status => <<"ESME_ROK">>,
      message_id => <<"this_could_be_a_message_id">>, sequence_number => 1,
      tlvs => [#{len => 25,tag => 1060,
                 val => <<109,121,95,109,101,115,115,97,103,101,95,112,97,
                          121,108,111,97,100,95,48,48,48,48,57,0>>}]}},
  {"data_sm_ms_msg_wait_facilities",
   "00 00 00 42 00 00 01 03 00 00 00 00 00 00 00 01 57 41 50 00 04 03 31 39 32 "
   "2E 31 36 38 2E 31 2E 31 00 04 08 31 36 38 2E 31 32 33 2E 32 33 34 2E 33 32 "
   "31 00 08 00 3F 00 30 00 01 83 02 0C 00 02 BF B3",
   #{command_id => <<"data_sm">>, command_length => 66,
     command_status => <<"ESME_ROK">>, data_coding => <<"63">>,
     dest_addr_npi => <<"National">>, dest_addr_ton => <<"Subscriber Number">>,
     destination_addr => <<"168.123.234.321">>, esm_class => 8,
     ms_msg_wait_facilities => 131,registered_delivery => 0,
     sar_msg_ref_num => 49075, sequence_number => 1, service_type => <<"WAP">>,
     source_addr => <<"192.168.1.1">>, source_addr_npi => <<"Data (X.121)">>,
     source_addr_ton => <<"Subscriber Number">>}},
  {"cancel_broadcast_sm_issue_30",
   "00 00 00 41 00 00 01 13 00 00 00 00 00 00 00 01 43 4D 54 00 74 68 69 73 5F "
   "63 6F 75 6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 06 06 31 "
   "36 38 2E 31 32 33 2E 32 33 34 2E 33 32 31 00",
   #{command_id => <<"cancel_broadcast_sm">>,command_length => 65,
     command_status => <<"ESME_ROK">>,
     message_id => <<"this_could_be_a_message_id">>, sequence_number => 1,
     service_type => <<"CMT">>, source_addr => <<"168.123.234.321">>,
     source_addr_npi => <<"Land Mobile (E.212)">>, source_addr_ton => <<"Abbreviated">>}},
  {"broadcast_sm_issue_42",
   "00 00 00 A9 00 00 01 11 00 00 00 00 00 00 00 01 47 55 54 53 00 00 0E 31 39 "
   "32 2E 31 36 38 2E 31 2E 31 00 74 68 69 73 5F 63 6F 75 6C 64 5F 62 65 5F 61 "
   "5F 6D 65 73 73 61 67 65 5F 69 64 00 04 39 39 30 37 32 34 31 37 35 34 34 34 "
   "30 30 30 52 00 39 39 30 38 32 33 31 36 35 33 34 33 30 30 30 52 00 00 0F 3F "
   "06 06 00 23 01 6D 79 5F 62 72 6F 61 64 63 61 73 74 5F 61 72 65 61 5F 69 64 "
   "65 6E 74 69 66 69 65 72 5F 30 30 30 30 37 06 01 00 03 00 00 33 06 04 00 02 "
   "00 00 06 05 00 03 08 00 03 13 0C 00 01 03 03 02 00 01 0B",
   #{command_id => <<"broadcast_sm">>, command_length => 169,
     command_status => <<"ESME_ROK">>, data_coding => <<"15">>,
     alert_on_message_delivery => 3,
     broadcast_area_identifier =>
        [#{details => <<"my_broadcast_area_identifier_00007">>, format => 1}],
     broadcast_content_type => #{network_type => 0,service => 51},
     broadcast_frequency_interval => #{number => 3,time_unit => 8},
     broadcast_rep_num => 0, callback_num_pres_ind => <<"\v">>,
     message_id => <<"this_could_be_a_message_id">>, priority_flag => 4,
     replace_if_present_flag => 0,
     schedule_delivery_time => <<"990724175444000R">>, sequence_number => 1,
     service_type => <<"GUTS">>, sm_default_msg_id => 63,
     source_addr => <<"192.168.1.1">>, source_addr_npi => <<"Internet (IP)">>,
     source_addr_ton => <<"Unknown">>,
     validity_period => <<"990823165343000R">>}},
 {"deliver_sm_issue_47",
  "00 00 01 60 00 00 00 05 00 00 00 00 00 00 00 01 57 41 50 00 01 06 31 36 38 "
  "2E 31 32 33 2E 32 33 34 2E 33 32 31 00 01 06 31 32 37 2E 30 2E 30 2E 31 00 "
  "01 03 00 39 39 30 37 32 34 31 37 35 34 34 34 30 30 30 52 00 39 39 30 36 32 "
  "35 31 38 35 35 34 35 30 30 30 2B 00 10 01 02 FF FF 31 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 32 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 33 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 34 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 35 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 36 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 37 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 38 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 39 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 41 20 54 68 69 73 20 69 "
  "73 20 61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 42 20 45 6E 64 02 05 00 "
  "01 FE",
  #{command_id => <<"deliver_sm">>,command_length => 352,
    command_status => <<"ESME_ROK">>,
    data_coding => <<"Octet unspecified (8-bit binary)">>,
    dest_addr_npi => <<"Land Mobile (E.212)">>,
    dest_addr_ton => <<"International">>,
    destination_addr => <<"127.0.0.1">>,esm_class => 1,
    priority_flag => 0,protocol_id => 3,registered_delivery => 16,
    replace_if_present_flag => 1,
    schedule_delivery_time => <<"990724175444000R">>,
    sequence_number => 1,service_type => <<"WAP">>,
    short_message => <<"1 This is a short message2 This is a short message3 "
                       "This is a short message4 This is a short message5 "
                       "This is a short message6 This is a short message7 "
                       "This is a short message8 This is a short message9 "
                       "This is a short messageA This is a short messageB "
                       "End">>,
    sm_default_msg_id => 255,source_addr => <<"168.123.234.321">>,
    source_addr_npi => <<"Land Mobile (E.212)">>,
    source_addr_ton => <<"International">>,user_response_code => 254,
    validity_period => <<"990625185545000+">>}},
 {"query_broadcast_sm_resp_issue_40",
  "00 00 00 5C 80 00 01 12 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 04 27 00 01 02 06 06 "
  "00 23 01 6D 79 5F 62 72 6F 61 64 63 61 73 74 5F 61 72 65 61 5F 69 64 65 6E "
  "74 69 66 69 65 72 5F 30 30 30 31 30 06 08 00 01 00",
  #{broadcast_area_identifier =>
    [#{details => <<"my_broadcast_area_identifier_00010">>, format => 1}],
    broadcast_area_success => [0], message_state => <<"DELIVERED">>,
    command_id => <<"query_broadcast_sm_resp">>, command_length => 92,
    command_status => <<"ESME_ROK">>, sequence_number => 1,
    message_id => <<"this_could_be_a_message_id">>}},
 {"data_sm_issue_50",
  "00 00 00 39 00 00 01 03 00 00 00 00 00 00 00 01 56 4D 4E 00 06 00 31 36 38 "
  "2E 30 2E 30 2E 31 00 06 0A 31 36 38 2E 30 2E 30 2E 31 00 02 0C 07 00 0F 00 "
  "01 08 00 06 00 01 00",
 #{command_id => <<"data_sm">>,command_length => 57,
   command_status => <<"ESME_ROK">>,
   data_coding => <<"Latin/Hebrew (ISO-8859-8)">>,
   dest_addr_npi => <<"ERMES">>,dest_addr_ton => <<"Abbreviated">>,
   dest_network_type => 0,destination_addr => <<"168.0.0.1">>,
   esm_class => 2,registered_delivery => 12,sequence_number => 1,
   service_type => <<"VMN">>,source_addr => <<"168.0.0.1">>,
   source_addr_npi => <<"Unknown">>,
   source_addr_ton => <<"Abbreviated">>,source_bearer_type => 8}},
 {"cancel_broadcast_sm_issue_52",
  "00 00 00 42 00 00 01 13 00 00 00 00 00 00 00 01 55 53 53 44 00 74 68 69 73 "
  "5F 63 6F 75 6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 06 0A "
  "31 36 38 2E 30 2E 30 2E 31 00 02 04 00 02 61 6A",
 #{command_id => <<"cancel_broadcast_sm">>,command_length => 66,command_status => <<"ESME_ROK">>,
   message_id => <<"this_could_be_a_message_id">>,sequence_number => 1,
   service_type => <<"USSD">>,source_addr => <<"168.0.0.1">>,
   source_addr_npi => <<"ERMES">>,source_addr_ton => <<"Abbreviated">>,
   user_message_reference => 24938}},
 {"query_sm_resp_issue_38",
  "00 00 00 3E 80 00 00 03 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 39 39 30 31 30 31 30 "
  "30 30 30 30 30 30 30 30 2B 00 05 31",
  #{command_id => <<"query_sm_resp">>,command_length => 62,
    command_status => <<"ESME_ROK">>, error_code => 49,
    final_date => <<"990101000000000+">>,
    message_id => <<"this_could_be_a_message_id">>,
    message_state => <<"UNDELIVERABLE">>,sequence_number => 1}},
 {"replace_sm_issue_55",
  "00 00 00 7A 00 00 00 07 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 05 09 31 39 32 2E 31 "
  "36 38 2E 31 2E 31 00 39 39 30 33 31 30 30 30 30 30 30 30 30 30 30 2D 00 39 "
  "39 30 30 30 30 30 30 30 30 30 30 30 30 30 52 00 00 1F 00 04 24 00 18 6D 79 "
  "5F 6D 65 73 73 61 67 65 5F 70 61 79 6C 6F 61 64 5F 30 30 30 30 38",
  #{command_id => <<"replace_sm">>,command_length => 122,
    command_status => <<"ESME_ROK">>,
    message_id => <<"this_could_be_a_message_id">>,
    message_payload => <<"my_message_payload_00008">>,
    registered_delivery => 0,
    schedule_delivery_time => <<"990310000000000-">>,
    sequence_number => 1,short_message => <<>>,
    sm_default_msg_id => 31,source_addr => <<"192.168.1.1">>,
    source_addr_npi => <<"Private">>,
    source_addr_ton => <<"Alphanumeric">>,
    validity_period => <<"990000000000000R">>}},
 {"data_sm_resp_issue_57",
  "00 00 00 53 80 00 01 03 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D 79 5F "
  "61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F 5F 74 65 "
  "78 74 00 04 25 00 01 00",
   #{additional_status_info_text =>
     <<"my_additional_status_info_text">>,
     command_id => <<"data_sm_resp">>,command_length => 83,
     command_status => <<"ESME_ROK">>,delivery_failure_reason => 0,
     message_id => <<"this_could_be_a_message_id">>,
     sequence_number => 1}},
 {"data_sm_issue_59",
  "00 00 00 52 00 00 01 03 00 00 00 00 00 00 00 01 47 55 54 53 00 06 01 31 36 "
  "38 2E 30 2E 30 2E 31 00 05 04 31 39 32 2E 31 36 38 2E 31 2E 31 00 00 05 1F "
  "02 0F 00 01 38 03 81 00 12 01 01 04 6D 79 5F 63 61 6C 6C 62 61 63 6B 5F 6E "
  "75 6D 00 06 00 01 06",
  #{callback_num => [#{addr_npi => 4,addr_ton => 1, digit_mode_indicator => 1,
                       number_digits => <<"my_callback_num">>}],
    command_id => <<"data_sm">>,command_length => 82,
    command_status => <<"ESME_ROK">>, data_coding => <<"31">>,
    dest_addr_npi => <<"Telex (F.69)">>, dest_addr_ton => <<"Alphanumeric">>,
    dest_network_type => 6, destination_addr => <<"192.168.1.1">>,
    esm_class => 0, registered_delivery => 5, sar_segment_seqnum => 56,
    sequence_number => 1, service_type => <<"GUTS">>,
    source_addr => <<"168.0.0.1">>, source_addr_npi => <<"ISDN (E163/E164)">>,
    source_addr_ton => <<"Abbreviated">>}},
 {"submit_sm_issue_61",
  "00 00 00 68 00 00 00 04 00 00 00 00 00 00 00 01 55 53 53 44 00 00 06 31 39 "
  "32 2E 31 2E 31 2E 31 30 00 02 03 31 39 32 2E 31 2E 31 2E 31 30 00 01 02 00 "
  "39 39 31 30 32 31 31 34 35 31 34 31 34 34 38 2B 00 39 39 30 39 32 32 31 35 "
  "35 32 34 32 30 30 30 52 00 01 00 00 1F 04 54 68 69 73 02 0B 00 02 30 5A 00 "
  "07 00 01 00",
  #{command_id => <<"submit_sm">>,command_length => 104,
    command_status => <<"ESME_ROK">>,
    data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"Data (X.121)">>,
    dest_addr_ton => <<"National">>,dest_bearer_type => 0,
    dest_port => 12378,destination_addr => <<"192.1.1.10">>,
    esm_class => 1,priority_flag => 0,protocol_id => 2,
    registered_delivery => 1,replace_if_present_flag => 0,
    schedule_delivery_time => <<"991021145141448+">>,
    sequence_number => 1,service_type => <<"USSD">>,
    short_message => <<"This">>,sm_default_msg_id => 31,
    source_addr => <<"192.1.1.10">>,
    source_addr_npi => <<"Land Mobile (E.212)">>,
    source_addr_ton => <<"Unknown">>,
    validity_period => <<"990922155242000R">>}},
 {"submit_sm_protocol_id_52_issue_65",
  "00 00 00 44 00 00 00 04 00 00 00 00 00 00 00 01 00 01 01 34 31 30 33 37 00 "
  "01 01 30 37 39 34 36 35 30 31 31 35 00 00 34 00 00 00 11 00 00 00 14 74 65 "
  "73 74 20 73 77 69 73 73 63 6F 6D 20 28 53 4D 50 50 29",
  #{command_id => <<"submit_sm">>,command_length => 68,
    command_status => <<"ESME_ROK">>,
    data_coding => <<"MC Specific">>,
    dest_addr_npi => <<"ISDN (E163/E164)">>,
    dest_addr_ton => <<"International">>,
    destination_addr => <<"0794650115">>,esm_class => 0,
    priority_flag => 0,protocol_id => 52,
    registered_delivery => 17,replace_if_present_flag => 0,
    schedule_delivery_time => <<>>,sequence_number => 1,
    service_type => <<>>,
    short_message => <<"test swisscom (SMPP)">>,
    sm_default_msg_id => 0,source_addr => <<"41037">>,
    source_addr_npi => <<"ISDN (E163/E164)">>,
    source_addr_ton => <<"International">>,
    validity_period => <<>>}},
  {"submit_multi_issue_46",
   "00 00 02 0D 00 00 00 21 00 00 00 00 00 00 00 01 57 41 50 00 06 0E 31 32 37 "
   "2E 30 2E 30 2E 31 00 0B 01 02 08 31 39 32 2E 31 36 38 2E 31 2E 31 00 02 64 "
   "69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 69 73 74 5F 23 31 00 02 64 69 73 74 "
   "72 69 62 75 74 69 6F 6E 5F 6C 69 73 74 5F 23 34 00 01 04 03 31 39 32 2E 31 "
   "2E 31 2E 31 30 00 02 64 69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 69 73 74 5F "
   "23 38 00 02 64 69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 69 73 74 5F 23 35 00 "
   "01 02 03 31 36 38 2E 30 2E 30 2E 31 00 02 64 69 73 74 72 69 62 75 74 69 6F "
   "6E 5F 6C 69 73 74 5F 23 32 00 01 04 0E 31 36 38 2E 30 2E 30 2E 31 00 02 64 "
   "69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 69 73 74 5F 23 36 00 01 04 09 31 36 "
   "38 2E 31 32 33 2E 32 33 34 2E 33 32 31 00 08 07 01 39 39 30 39 32 32 31 35 "
   "35 32 34 32 30 30 30 52 00 00 0C 00 FF 0F FF 31 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 32 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 33 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 34 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 35 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 36 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 37 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 38 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 39 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 41 20 54 68 69 73 20 69 73 20 "
   "61 20 73 68 6F 72 74 20 6D 65 73 73 61 67 65 42 20 45 6E 64 02 05 00 01 27",
   #{command_id => <<"submit_multi">>, command_length => 525,
     command_status => <<"ESME_ROK">>, data_coding => <<"255">>,
     dest_address => [
        #{dest_addr_npi => 8,dest_addr_ton => 2,dest_flag => 1,
          destination_addr => <<"192.168.1.1">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#1">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#4">>},
        #{dest_addr_npi => 3,dest_addr_ton => 4,dest_flag => 1,
          destination_addr => <<"192.1.1.10">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#8">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#5">>},
        #{dest_addr_npi => 3,dest_addr_ton => 2,dest_flag => 1,
          destination_addr => <<"168.0.0.1">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#2">>},
        #{dest_addr_npi => 14,dest_addr_ton => 4,dest_flag => 1,
          destination_addr => <<"168.0.0.1">>},
        #{dest_flag => 2,dl_name => <<"distribution_list_#6">>},
        #{dest_addr_npi => 9,dest_addr_ton => 4,dest_flag => 1,
          destination_addr => <<"168.123.234.321">>}
	 ],
     esm_class => 8, priority_flag => 1, protocol_id => 7,
     registered_delivery => 12, replace_if_present_flag => 0,
     schedule_delivery_time => <<"990922155242000R">>,
     sequence_number => 1, service_type => <<"WAP">>,
     short_message => <<"1 This is a short message2 This is a short message3 "
                        "This is a short message4 This is a short message5 "
                        "This is a short message6 This is a short message7 "
                        "This is a short message8 This is a short message9 "
                        "This is a short messageA This is a short messageB "
                        "End">>,
     sm_default_msg_id => 15, source_addr => <<"127.0.0.1">>,
     source_addr_npi => <<"Internet (IP)">>,
     source_addr_ton => <<"Abbreviated">>, user_response_code => 39,
     validity_period => <<>>}},
  {"broadcast_sm_#69",
   "00 00 00 B1 00 00 01 11 00 00 00 00 00 00 00 01 00 02 08 31 32 37 2E 30 "
   "2E 30 2E 31 00 74 68 69 73 5F 63 6F 75 6C 64 5F 62 65 5F 61 5F 6D 65 73 "
   "73 61 67 65 5F 69 64 00 04 00 39 39 30 33 31 30 30 30 30 30 30 30 30 30 "
   "30 52 00 01 FF 7F 06 06 00 23 02 6D 79 5F 62 72 6F 61 64 63 61 73 74 5F "
   "61 72 65 61 5F 69 64 65 6E 74 69 66 69 65 72 5F 30 30 30 31 30 06 01 00 "
   "03 00 00 81 06 04 00 02 00 00 06 05 00 03 0B 00 05 06 02 00 24 6D 79 5F "
   "62 72 6F 61 64 63 61 73 74 5F 63 6F 6E 74 65 6E 74 5F 74 79 70 65 5F 69 "
   "6E 66 6F 5F 30 30 30 31 32",
   #{broadcast_area_identifier => [
	   #{details => <<"my_broadcast_area_identifier_00010">>, format => 2}
	 ],
     broadcast_content_type => #{network_type => 0,service => 129},
     broadcast_content_type_info => <<"my_broadcast_content_type_info_00012">>,
     broadcast_frequency_interval => #{number => 5,time_unit => 11},
     broadcast_rep_num => 0, command_id => <<"broadcast_sm">>,
     command_length => 177, command_status => <<"ESME_ROK">>,
     data_coding => <<"255">>, message_id => <<"this_could_be_a_message_id">>,
     priority_flag => 4, replace_if_present_flag => 1,
     schedule_delivery_time => <<>>, sequence_number => 1, service_type => <<>>,
     sm_default_msg_id => 127, source_addr => <<"127.0.0.1">>,
     source_addr_npi => <<"National">>, source_addr_ton => <<"National">>,
     validity_period => <<"990310000000000R">>}},
  {"broadcast_sm_resp_#70",
   "00 00 00 59 80 00 01 11 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 06 06 00 2A 00 "
   "6D 79 5F 66 61 69 6C 65 64 5F 62 72 6F 61 64 63 61 73 74 5F 61 72 65 61 "
   "5F 69 64 65 6E 74 69 66 69 65 72 5F 30 30 30 30 38",
   #{command_id => <<"broadcast_sm_resp">>, command_length => 89,
     command_status => <<"ESME_ROK">>,
     failed_broadcast_area_identifier => [
		#{details => <<"my_failed_broadcast_area_identifier_00008">>,
          format => 0}
	 ],
     message_id => <<"this_could_be_a_message_id">>, sequence_number => 1}},
  {"submit_multi_#79",
   "00 00 01 0D 00 00 00 21 00 00 00 00 00 00 00 01 47 55 54 53 00 01 04 31 "
   "39 32 2E 31 2E 31 2E 31 30 00 0A 02 64 69 73 74 72 69 62 75 74 69 6F 6E "
   "5F 6C 73 74 5F 23 36 00 01 06 12 31 36 38 2E 31 32 33 2E 32 33 34 2E 33 "
   "32 31 00 02 64 69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 73 74 5F 23 34 00 "
   "01 01 0A 31 39 32 2E 31 36 38 2E 31 2E 31 00 02 64 69 73 74 72 69 62 75 "
   "74 69 6F 6E 5F 6C 73 74 5F 23 31 00 02 64 69 73 74 72 69 62 75 74 69 6F "
   "6E 5F 6C 73 74 5F 23 38 00 01 06 00 31 36 38 2E 30 2E 30 2E 31 00 02 64 "
   "69 73 74 72 69 62 75 74 69 6F 6E 5F 6C 73 74 5F 23 35 00 02 64 69 73 74 "
   "72 69 62 75 74 69 6F 6E 5F 6C 73 74 5F 23 32 00 01 05 12 31 36 38 2E 30 "
   "2E 30 2E 31 00 40 03 03 39 39 30 34 32 37 32 30 30 30 30 30 30 30 30 2B "
   "00 39 39 30 34 32 37 32 30 30 30 30 30 30 30 30 2B 00 01 00 3F 03 01 54 "
   "02 0F 00 01 C0",
   #{command_id => <<"submit_multi">>,command_length => 269,
     command_status => <<"ESME_ROK">>, data_coding => <<"63">>,
     dest_address => [
        #{dest_flag => 2,dl_name => <<"distribution_lst_#6">>},
        #{dest_addr_npi => 18,dest_addr_ton => 6,dest_flag => 1,
          destination_addr => <<"168.123.234.321">>},
        #{dest_flag => 2,dl_name => <<"distribution_lst_#4">>},
        #{dest_addr_npi => 10,dest_addr_ton => 1,dest_flag => 1,
          destination_addr => <<"192.168.1.1">>},
        #{dest_flag => 2,dl_name => <<"distribution_lst_#1">>},
        #{dest_flag => 2,dl_name => <<"distribution_lst_#8">>},
        #{dest_addr_npi => 0,dest_addr_ton => 6,dest_flag => 1,
          destination_addr => <<"168.0.0.1">>},
        #{dest_flag => 2,dl_name => <<"distribution_lst_#5">>},
        #{dest_flag => 2,dl_name => <<"distribution_lst_#2">>},
        #{dest_addr_npi => 18,dest_addr_ton => 5,dest_flag => 1,
          destination_addr => <<"168.0.0.1">>}
     ],
     esm_class => 64, priority_flag => 3, protocol_id => 3,
     registered_delivery => 1, replace_if_present_flag => 0,
     sar_segment_seqnum => 192, sequence_number => 1,
     schedule_delivery_time => <<"990427200000000+">>,
     service_type => <<"GUTS">>, short_message => <<"T">>,
     sm_default_msg_id => 3, source_addr => <<"192.1.1.10">>,
     source_addr_npi => <<"Telex (F.69)">>,
     source_addr_ton => <<"International">>,
     validity_period => <<"990427200000000+">>}},
  {"query_broadcast_sm_resp_#78",
   "00 00 00 5C 80 00 01 12 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 04 27 00 01 09 "
   "06 06 00 23 01 6D 79 5F 62 72 6F 61 64 63 61 73 74 5F 61 72 65 61 5F 69 "
   "64 65 6E 74 69 66 69 65 72 5F 30 30 30 31 32 06 08 00 01 49",
   #{broadcast_area_identifier => [
        #{details => <<"my_broadcast_area_identifier_00012">>, format => 1}
     ],
     broadcast_area_success => "I", command_id => <<"query_broadcast_sm_resp">>,
     command_length => 92, command_status => <<"ESME_ROK">>,
     message_id => <<"this_could_be_a_message_id">>, message_state => <<"SKIPPED">>,
     sequence_number => 1}},
  {"deliver_sm_resp_#77",
   "00 00 00 55 80 00 00 05 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D "
   "79 5F 61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F "
   "5F 74 65 78 74 00 04 23 00 03 05 38 32",
   #{additional_status_info_text => <<"my_additional_status_info_text">>,
     command_id => <<"deliver_sm_resp">>, command_length => 85,
     command_status => <<"ESME_ROK">>, sequence_number => 1,
     message_id => <<"this_could_be_a_message_id">>,
     network_error_code => #{error => 14386,type => 5}}},
  {"submit_sm_resp_#76",
   "00 00 00 53 80 00 00 04 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D "
   "79 5F 61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F "
   "5F 74 65 78 74 00 04 20 00 01 01",
   #{additional_status_info_text => <<"my_additional_status_info_text">>,
     command_id => <<"submit_sm_resp">>, command_length => 83,
	 command_status => <<"ESME_ROK">>, dpf_result => 1, sequence_number => 1,
	 message_id => <<"this_could_be_a_message_id">>}},
  {"submit_multi_resp_#75",
   "00 00 00 54 80 00 00 21 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 00 1D 00 1F "
   "6D 79 5F 61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 "
   "6F 5F 74 65 78 74 00 04 20 00 01 00",
   #{additional_status_info_text => <<"my_additional_status_info_text">>,
     command_id => <<"submit_multi_resp">>, command_length => 84,
     command_status => <<"ESME_ROK">>, dpf_result => 0, sequence_number => 1,
 	 message_id => <<"this_could_be_a_message_id">>, unsuccess_sme => <<>>}},
  {"data_sm_resp_#74",
   "00 00 00 53 80 00 01 03 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 "
   "6C 64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 00 1D 00 1F 6D "
   "79 5F 61 64 64 69 74 69 6F 6E 61 6C 5F 73 74 61 74 75 73 5F 69 6E 66 6F "
   "5F 74 65 78 74 00 04 20 00 01 00",
   #{additional_status_info_text => <<"my_additional_status_info_text">>,
     command_id => <<"data_sm_resp">>,command_length => 83,
	 command_status => <<"ESME_ROK">>, dpf_result => 0, sequence_number => 1,
	 message_id => <<"this_could_be_a_message_id">>}},
  {"submit_sm_#73",
   "00 00 00 4E 00 00 00 04 00 00 00 00 00 00 00 01 57 41 50 00 01 08 31 32 "
   "37 2E 30 2E 30 2E 31 00 01 06 31 39 32 2E 31 2E 31 2E 31 30 00 C0 03 01 "
   "39 39 31 30 32 31 31 34 35 31 34 31 34 34 38 2B 00 00 01 00 0F 0F 01 54 "
   "00 08 00 02 42 00",
   #{command_id => <<"submit_sm">>, command_length => 78,
     command_status => <<"ESME_ROK">>, data_coding => <<"15">>,
             dest_addr_npi => <<"Land Mobile (E.212)">>,
             dest_addr_ton => <<"International">>,
             dest_telematics_id => #{protocol_id => 66,reserved => 0},
             destination_addr => <<"192.1.1.10">>,esm_class => 192,
             priority_flag => 1,protocol_id => 3,registered_delivery => 1,
             replace_if_present_flag => 0,
             schedule_delivery_time => <<"991021145141448+">>,
             sequence_number => 1,service_type => <<"WAP">>,
             short_message => <<"T">>,sm_default_msg_id => 15,
             source_addr => <<"127.0.0.1">>,source_addr_npi => <<"National">>,
             source_addr_ton => <<"International">>,validity_period => <<>>}},
  {"deliver_sm_#72",
   "00 00 00 72 00 00 00 05 00 00 00 00 00 00 00 01 43 4D 54 00 00 06 31 39 "
   "32 2E 31 36 38 2E 31 2E 31 00 06 01 31 36 38 2E 30 2E 30 2E 31 00 10 01 "
   "00 39 39 30 34 32 37 32 30 30 30 30 30 30 30 30 2B 00 39 39 30 35 32 36 "
   "31 39 35 36 30 30 30 30 30 52 00 04 01 FF 07 07 54 68 69 73 20 69 73 06 "
   "0E 00 0F 33 31 31 32 34 30 30 32 30 30 35 30 32 34 00",
   #{command_id => <<"deliver_sm">>, command_length => 114,
     command_status => <<"ESME_ROK">>, data_coding => <<"255">>,
	 dest_addr_npi => <<"ISDN (E163/E164)">>, esm_class => 16,
	 dest_addr_ton => <<"Abbreviated">>, priority_flag => 0,
	 dest_network_id => <<"31124002005024">>,protocol_id => 1,
	 destination_addr => <<"168.0.0.1">>,registered_delivery => 4,
	 replace_if_present_flag => 1, sequence_number => 1,
	 schedule_delivery_time => <<"990427200000000+">>,
	 service_type => <<"CMT">>, short_message => <<"This is">>,
	 sm_default_msg_id => 7, source_addr => <<"192.168.1.1">>,
	 source_addr_npi => <<"Land Mobile (E.212)">>,
	 source_addr_ton => <<"Unknown">>,
	 validity_period => <<"990526195600000R">>}},
  {"data_sm_#71",
   "00 00 00 46 00 00 01 03 00 00 00 00 00 00 00 01 56 4D 41 00 06 08 31 36 "
   "38 2E 31 32 33 2E 32 33 34 2E 33 32 31 00 01 00 31 36 38 2E 31 32 33 2E "
   "32 33 34 2E 33 32 31 00 40 00 02 02 04 00 02 31 A5 03 04 00 01 59",
   #{command_id => <<"data_sm">>, command_length => 70, esm_class => 64,
     command_status => <<"ESME_ROK">>, dest_addr_ton => <<"International">>,
	 data_coding => <<"Octet unspecified (8-bit binary)">>,
	 dest_addr_npi => <<"Unknown">>, destination_addr => <<"168.123.234.321">>,
	 number_of_messages => 89, registered_delivery => 0, sequence_number => 1,
	 service_type => <<"VMA">>, source_addr => <<"168.123.234.321">>,
	 source_addr_npi => <<"National">>, user_message_reference => 12709,
	 source_addr_ton => <<"Abbreviated">>}},
  {"deliver_sm_#85",
   "00 00 00 78 00 00 00 05 00 00 00 00 00 00 00 01 43 50 54 00 03 00 31 36 "
   "38 2E 30 2E 30 2E 31 00 05 06 31 32 37 2E 30 2E 30 2E 31 00 40 03 02 39 "
   "39 30 33 31 30 30 30 30 30 30 30 30 30 30 52 00 39 39 31 30 32 31 31 34 "
   "35 31 34 31 34 34 38 2B 00 01 00 03 1F 04 54 68 69 73 03 03 00 15 1F 6D "
   "79 5F 63 61 6C 6C 62 61 63 6B 5F 6E 75 6D 5F 61 74 61 67 13 80 00 01 03",
   #{callback_num_atag => [
	   	#{data_coding => 31, display_characters => <<"my_callback_num_atag">>}
	 ],
	 command_id => <<"deliver_sm">>, command_length => 120,
	 command_status => <<"ESME_ROK">>, priority_flag => 2, protocol_id => 3,
	 data_coding => <<"Latin 1 (ISO-8859-1)">>, its_reply_type => 3,
	 dest_addr_npi => <<"Land Mobile (E.212)">>, esm_class => 64,
	 dest_addr_ton => <<"Alphanumeric">>, destination_addr => <<"127.0.0.1">>,
	 registered_delivery => 1, replace_if_present_flag => 0,
	 schedule_delivery_time => <<"990310000000000R">>, sm_default_msg_id => 31,
	 sequence_number => 1, service_type => <<"CPT">>,
	 short_message => <<"This">>, source_addr => <<"168.0.0.1">>,
	 source_addr_npi => <<"Unknown">>,
	 source_addr_ton => <<"Network Specific">>,
	 validity_period => <<"991021145141448+">>}},
  {"data_sm_#87",
   "00 00 00 37 00 00 01 03 00 00 00 00 00 00 00 01 43 50 54 00 05 0E 31 32 "
   "37 2E 30 2E 30 2E 31 00 04 09 31 39 32 2E 31 36 38 2E 31 2E 31 00 20 04 "
   "3F 12 03 00 02 29 C0",
   #{command_id => <<"data_sm">>, command_length => 55,
     command_status => <<"ESME_ROK">>, data_coding => <<"63">>,
	 dest_addr_npi => <<"Private">>, dest_addr_ton => <<"Subscriber Number">>,
	 destination_addr => <<"192.168.1.1">>, esm_class => 32,
	 registered_delivery => 4, sequence_number => 1, service_type => <<"CPT">>,
	 sms_signal => 10688, source_addr => <<"127.0.0.1">>,
	 source_addr_npi => <<"Internet (IP)">>, source_addr_ton => <<"Alphanumeric">>}}
]).

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

-define(ENOCDE_MSG_DECODE_MSG_TEST,
    [{"empty", 
      #{short_message => <<>>, data_coding => ?ENCODING_SCHEME_LATIN_1},
      #{short_message => <<>>, data_coding => ?ENCODING_SCHEME_LATIN_1}},
     {"emty_ucs2", 
      #{short_message => <<>>, data_coding => ?ENCODING_SCHEME_UCS2},
      #{short_message => <<>>, data_coding => ?ENCODING_SCHEME_UCS2}},
     {"ucs2_bigger_eur", 
      #{data_coding => ?ENCODING_SCHEME_UCS2, short_message => <<"Abc₭"/utf8>>},
      #{data_coding => ?ENCODING_SCHEME_UCS2, 
        short_message => unicode:characters_to_binary(<<"Abc₭"/utf8>>, utf8, utf16)}},
     {"base64", 
      #{data_coding => ?ENCODING_SCHEME_MC_SPECIFIC, short_message => <<"Test">>},
      #{data_coding => ?ENCODING_SCHEME_MC_SPECIFIC, short_message => <<"Test">>}}
   ]
).

-define(EMOJI_TEST, %[{Title, Object, Expected}]
    [{"1-part all emojis", 
      #{command_id => <<"submit_sm">>,command_length => 33,
        command_status => <<"ESME_ROK">>,
        data_coding => <<"UCS2 (ISO/IEC-10646)">>,
        dest_addr_npi => <<"ISDN (E163/E164)">>,
        dest_addr_ton => <<"International">>,
        destination_addr => <<>>,esm_class => 0,priority_flag => 0,
        protocol_id => 0,registered_delivery => 0,
        replace_if_present_flag => 0,schedule_delivery_time => <<>>,
        sequence_number => 0,service_type => <<>>,
        short_message => <<"🙂🙃🙄🙅🙆🙇🙈🙉🙊🙋🙌🙍🙎🙏"/utf8>>,
        sm_default_msg_id => 0, source_addr => <<>>,
        source_addr_npi => <<"ISDN (E163/E164)">>,
        source_addr_ton => <<"International">>,
        validity_period => <<>>},
      #{command_id => <<"submit_sm">>,command_length => 89,
        command_status => <<"ESME_ROK">>,
        data_coding => <<"UCS2 (ISO/IEC-10646)">>,
        dest_addr_npi => <<"ISDN (E163/E164)">>,
        dest_addr_ton => <<"International">>,
        destination_addr => <<>>,esm_class => 0,priority_flag => 0,
        protocol_id => 0,registered_delivery => 0,
        replace_if_present_flag => 0,schedule_delivery_time => <<>>,
        sequence_number => 0,service_type => <<>>,
        short_message =>
            <<"\\uD83D\\uDE42\\uD83D\\uDE43\\uD83D\\uDE44\\uD83D\\uDE45\\uD83D"
              "\\uDE46\\uD83D\\uDE47\\uD83D\\uDE48\\uD83D\\uDE49\\uD83D\\uDE4A"
              "\\uD83D\\uDE4B\\uD83D\\uDE4C\\uD83D\\uDE4D\\uD83D\\uDE4E\\uD83D"
              "\\uDE4F">>,
        sm_default_msg_id => 0,source_addr => <<>>,
        source_addr_npi => <<"ISDN (E163/E164)">>,
        source_addr_ton => <<"International">>, validity_period => <<>>}
   }
  ]).

-endif. % _TESTS_HRL_