-module(smpp).
-include("smpp_globals.hrl").

-export([pack/1, unpack/1, unpack_map/1, unpack/2, json2internal/1,
         internal2json/1, encode/1, decode/1]).
-export([err/1,cmd/1,cmdstr/1]).

json2internal(SMPP) when is_map(SMPP) ->
    maps:fold(fun(K,V,M) when is_binary(K) ->
                      AK = b2a(K),
                      M#{AK => if is_binary(V) -> binary_to_list(V);
                                  is_list(V) ->
                                      lists:map(fun(VI) when is_map(VI) ->
                                                        json2internal(VI);
                                                   (VI) -> VI
                                                end, V);
                                  is_map(V) -> json2internal(V);
                                  V == null -> [];
                                  V == true -> 1;
                                  V == false -> 0;
                                  true -> V
                               end}
              end, #{}, SMPP).

internal2json(SMPP) when is_map(SMPP) ->
    maps:map(
        fun(_, V) when is_list(V) -> list_to_binary(V);
           (_, V) -> V
        end,
        case SMPP of
            #{data_coding := DC, short_message := SM}
              when DC == ?ENCODING_SCHEME_LATIN_1;
                   DC == ?ENCODING_SCHEME_IA5_ASCII;
                   DC == ?ENCODING_SCHEME_MC_SPECIFIC ->
                SMPP#{data_coding => <<"utf8_compact">>,
                      short_message => unicode:characters_to_binary(SM)};
            #{data_coding := ?ENCODING_SCHEME_UCS2, short_message := SM} ->
                SMPP#{data_coding => <<"utf8">>,
                      short_message => ucs2_to_utf16(SM)};
            #{data_coding := _, short_message := SM} ->
                SMPP#{short_message => base64:encode(list_to_binary(SM))};
            SMPP -> SMPP
        end).

ucs2_to_utf16({cp, CPList}) ->
    case unicode:characters_to_binary(CPList, utf16, utf8) of
        {error, ConvertedBin, [[Failed]|Rest]} ->
            <<ConvertedBin/binary, (Failed bsr 8), (Failed band 16#00FF),
              (ucs2_to_utf16({cp, Rest}))/binary>>;
        ConvertedBin when is_binary(ConvertedBin) -> ConvertedBin
    end;
ucs2_to_utf16(ShortMessage) ->
    CPList = ucs2_to_utf16_cp(ShortMessage, []),
    io:format(user, "{~p,~p} ShortMessage ~w~n",
              [?MODULE, ?LINE, ShortMessage]),
    ucs2_to_utf16({cp, CPList}).

ucs2_to_utf16_cp([], Result) -> lists:reverse(Result);
% Erlang DOC: An integer in the range 16#D800 to 16#DFFF (invalid range
%  reserved for UTF-16 surrogate pairs)
ucs2_to_utf16_cp([A,B|Rest], Result) when A >= 16#D8, A =< 16#DF ->
    ucs2_to_utf16_cp(
      Rest, lists:reverse(
              lists:flatten(
                [$\\, $u,
                 io_lib:format("~2.16.0B~2.16.0B", [A,B])]
               )) ++ Result);
ucs2_to_utf16_cp([A,B|Rest], Result) ->
    ucs2_to_utf16_cp(Rest, [(A bsl 8) + B | Result]).

b2a(<<"password">>) -> password;
b2a(<<"addr_npi">>) -> addr_npi;
b2a(<<"addr_ton">>) -> addr_ton;
b2a(<<"esm_class">>) -> esm_class;
b2a(<<"sm_length">>) -> sm_length;
b2a(<<"system_id">>) -> system_id;
b2a(<<"message_id">>) -> message_id;
b2a(<<"command_id">>) -> command_id;
b2a(<<"error_code">>) -> error_code;
b2a(<<"final_date">>) -> final_date;
b2a(<<"data_coding">>) -> data_coding;
b2a(<<"system_type">>) -> system_type;
b2a(<<"source_addr">>) -> source_addr;
b2a(<<"protocol_id">>) -> protocol_id;
b2a(<<"service_type">>) -> service_type;
b2a(<<"dest_addr_npi">>) -> dest_addr_npi;
b2a(<<"dest_addr_ton">>) -> dest_addr_ton;
b2a(<<"short_message">>) -> short_message;
b2a(<<"message_state">>) -> message_state;
b2a(<<"address_range">>) -> address_range;
b2a(<<"priority_flag">>) -> priority_flag;
b2a(<<"command_status">>) -> command_status;
b2a(<<"sequence_number">>) -> sequence_number;
b2a(<<"source_addr_npi">>) -> source_addr_npi;
b2a(<<"source_addr_ton">>) -> source_addr_ton;
b2a(<<"validity_period">>) -> validity_period;
b2a(<<"sar_msg_ref_num">>) -> sar_msg_ref_num;
b2a(<<"destination_addr">>) -> destination_addr;
b2a(<<"sm_default_msg_id">>) -> sm_default_msg_id;
b2a(<<"interface_version">>) -> interface_version;
b2a(<<"dest_addr_subunit">>) -> dest_addr_subunit;
b2a(<<"sar_segment_seqnum">>) -> sar_segment_seqnum;
b2a(<<"sar_total_segments">>) -> sar_total_segments;
b2a(<<"registered_delivery">>) -> registered_delivery;
b2a(<<"schedule_delivery_time">>) -> schedule_delivery_time;
b2a(<<"replace_if_present_flag">>) -> replace_if_present_flag.

pack(#{command_id := CmdId, command_status := Status, sequence_number := SeqNum} = SMPP) ->
    NewSMPP = maps:without([command_id, command_status, sequence_number], SMPP),
    Body = maps:fold(fun map_to_pl/3, [], NewSMPP),
    case pack({CmdId, Status, SeqNum, Body}) of
        {ok, PduBinParts} when is_list(PduBinParts) ->
            {ok, list_to_binary(PduBinParts)};
        Error -> Error
    end;
pack({CmdId, Status, SeqNum, Body} = SMPP)
  when is_integer(CmdId), is_integer(Status), is_integer(SeqNum),
       is_list(Body) ->
  smpp_operation:pack(SMPP).

unpack(Bin) -> unpack(Bin, []).
unpack_map(Bin) -> unpack(Bin, [return_maps]).
unpack(Bin, Opts) ->
    {ok, SMPP} = case smpp_operation:unpack(Bin) of
                     {error, _, S, _} = Error ->
                         io:format("Unpack error ~p~n", [err(S)]),
                         Error;
                     Ok -> Ok
                 end,
    {CmdId, Status, SeqNum, Body} = SMPP,
    case lists:member(return_maps, Opts) of
        true ->
            SMPPMap = lists:foldl(fun list_to_map/2, #{}, Body),
            SMPPMap#{command_id => CmdId, command_status => Status, sequence_number => SeqNum};
        _ ->
            Hd = [{command_id, CmdId}, {command_status, Status}, {sequence_number, SeqNum}],
            Hd ++ lists:foldl(fun list_to_pl/2, [], Body)
    end.

list_to_map({K, V}, Acc) when is_tuple(V) ->
    Acc#{K => rec_to_map(V)};
list_to_map({K, V}, Acc) when is_list(V) ->
    case V of
        [H|_T] when is_tuple(H) -> 
            Acc#{K => [rec_to_map(R) || R <- V]};
        _ -> Acc#{K => V}
    end;
list_to_map({K, V}, Acc) ->
    Acc#{K => V}.

list_to_pl({K, V}, Acc) when is_tuple(V) ->
    [{K, rec_to_pl(V)} | Acc];
list_to_pl({K, V}, Acc) when is_list(V) ->
    case V of
        [H|_T] when is_tuple(H) -> 
            [{K, [rec_to_pl(R) || R <- V]} | Acc];
        _ -> [{K, V} | Acc]
    end;
list_to_pl({K, V}, Acc) ->
    [{K, V} | Acc].

map_to_pl(K, V, Acc) when is_map(V) ->
    [{K, map_to_rec(K, V)} | Acc];
map_to_pl(K, V, Acc) when is_list(V) ->
    case V of
        [H|_T] when is_map(H) -> 
            [{K, [map_to_rec(K, M) || M <- V]} | Acc];
        _ ->
            [{K, V} | Acc]
    end;
map_to_pl(K, V, Acc) ->
    [{K, V} | Acc].
     
rec_to_map(Rec) ->
    maps:from_list(rec_to_pl(Rec)).

rec_to_pl(Rec) when is_tuple(Rec) ->
    Fields = rec_info(element(1, Rec)),
    [{lists:nth(N, Fields), element(N+1, Rec)} || N <- lists:seq(1, length(Fields))].

map_to_rec(Type, Map) when is_map(Map) ->
    Rec = rec_type(Type),
    io:format("map to rec : ~p~n", [list_to_tuple([Rec | [maps:get(K, Map) || K <- rec_info(Rec)]])]),
    list_to_tuple([Rec | [maps:get(K, Map) || K <- rec_info(Rec)]]).

err(?ESME_ROK)->                 {'ESME_ROK',                   "ESME_ROK",                 "No Error"};
err(?ESME_RINVMSGLEN)->          {'ESME_RINVMSGLEN',            "ESME_RINVMSGLEN",          "Message Length is invalid"};
err(?ESME_RINVCMDLEN)->          {'ESME_RINVCMDLEN',            "ESME_RINVCMDLEN",          "Command Length is invalid"};
err(?ESME_RINVCMDID)->           {'ESME_RINVCMDID',             "ESME_RINVCMDID",           "Invalid Command ID"};
err(?ESME_RINVBNDSTS)->          {'ESME_RINVBNDSTS',            "ESME_RINVBNDSTS",          "Incorrect BIND Status for given command"};
err(?ESME_RALYBND)->             {'ESME_RALYBND',               "ESME_RALYBND",             "ESME Already in Bound State"};
err(?ESME_RINVPRTFLG)->          {'ESME_RINVPRTFLG',            "ESME_RINVPRTFLG",          "Invalid Priority Flag"};
err(?ESME_RINVREGDLVFLG)->       {'ESME_RINVREGDLVFLG',         "ESME_RINVREGDLVFLG",       "Invalid Registered Delivery Flag"};
err(?ESME_RSYSERR)->             {'ESME_RSYSERR',               "ESME_RSYSERR",             "System Error"};
err(?ESME_RINVSRCADR)->          {'ESME_RINVSRCADR',            "ESME_RINVSRCADR",          "Invalid Source Address"};
err(?ESME_RINVDSTADR)->          {'ESME_RINVDSTADR',            "ESME_RINVDSTADR",          "Invalid Dest Addr"};
err(?ESME_RINVMSGID)->           {'ESME_RINVMSGID',             "ESME_RINVMSGID",           "Message ID is invalid"};
err(?ESME_RBINDFAIL)->           {'ESME_RBINDFAIL',             "ESME_RBINDFAIL",           "Bind Failed"};
err(?ESME_RINVPASWD)->           {'ESME_RINVPASWD',             "ESME_RINVPASWD",           "Invalid Password"};
err(?ESME_RINVSYSID)->           {'ESME_RINVSYSID',             "ESME_RINVSYSID",           "Invalid System ID"};
err(?ESME_RCANCELFAIL)->         {'ESME_RCANCELFAIL',           "ESME_RCANCELFAIL",         "Cancel SM Failed"};
err(?ESME_RREPLACEFAIL)->        {'ESME_RREPLACEFAIL',          "ESME_RREPLACEFAIL",        "Replace SM Failed"};
err(?ESME_RMSGQFUL)->            {'ESME_RMSGQFUL',              "ESME_RMSGQFUL",            "Message Queue Full"};
err(?ESME_RINVSERTYP)->          {'ESME_RINVSERTYP',            "ESME_RINVSERTYP",          "Invalid Service Type"};
err(?ESME_RINVNUMDESTS)->        {'ESME_RINVNUMDESTS',          "ESME_RINVNUMDESTS",        "Invalid destinations number"};
err(?ESME_RINVDLNAME)->          {'ESME_RINVDLNAME',            "ESME_RINVDLNAME",          "Invalid Distribution List name"};
err(?ESME_RINVDESTFLAG)->        {'ESME_RINVDESTFLAG',          "ESME_RINVDESTFLAG",        "Invalid Destination flag (submit_multi)"};
err(?ESME_RINVSUBREP)->          {'ESME_RINVSUBREP',            "ESME_RINVSUBREP",          "Invalid submit with replace"};
err(?ESME_RINVESMCLASS)->        {'ESME_RINVESMCLASS',          "ESME_RINVESMCLASS",        "Invalid esm_class field data"};
err(?ESME_RCNTSUBDL)->           {'ESME_RCNTSUBDL',             "ESME_RCNTSUBDL",           "Cannot Submit to Distribution List"};
err(?ESME_RSUBMITFAIL)->         {'ESME_RSUBMITFAIL',           "ESME_RSUBMITFAIL",         "submit_sm or submit_multi failed"};
err(?ESME_RINVSRCTON)->          {'ESME_RINVSRCTON',            "ESME_RINVSRCTON",          "Invalid Source address TON"};
err(?ESME_RINVSRCNPI)->          {'ESME_RINVSRCNPI',            "ESME_RINVSRCNPI",          "Invalid Source address NPI"};
err(?ESME_RINVDSTTON)->          {'ESME_RINVDSTTON',            "ESME_RINVDSTTON",          "Invalid Destination addr TON"};
err(?ESME_RINVDSTNPI)->          {'ESME_RINVDSTNPI',            "ESME_RINVDSTNPI",          "Invalid Destination addr NPI"};
err(?ESME_RINVSYSTYP)->          {'ESME_RINVSYSTYP',            "ESME_RINVSYSTYP",          "Invalid system_type field"};
err(?ESME_RINVREPFLAG)->         {'ESME_RINVREPFLAG',           "ESME_RINVREPFLAG",         "Invalid replace_if_present Flag"};
err(?ESME_RINVNUMMSGS)->         {'ESME_RINVNUMMSGS',           "ESME_RINVNUMMSGS",         "Invalid number of messages"};
err(?ESME_RTHROTTLED)->          {'ESME_RTHROTTLED',            "ESME_RTHROTTLED",          "Throttling error (ESME has exceeded allowed msg limits)"};
err(?ESME_RINVSCHED)->           {'ESME_RINVSCHED',             "ESME_RINVSCHED",           "Invalid Scheduled Delivery Time"};
err(?ESME_RINVEXPIRY)->          {'ESME_RINVEXPIRY',            "ESME_RINVEXPIRY",          "Invalid message validity period (Expiry time)"};
err(?ESME_RINVDFTMSGID)->        {'ESME_RINVDFTMSGID',          "ESME_RINVDFTMSGID",        "Predefined Message Invalid or Not Found"};
err(?ESME_RX_T_APPN)->           {'ESME_RX_T_APPN',             "ESME_RX_T_APPN",           "ESME Receiver Temporary App Err"};
err(?ESME_RX_P_APPN)->           {'ESME_RX_P_APPN',             "ESME_RX_P_APPN",           "ESME Receiver Permanent App Err"};
err(?ESME_RX_R_APPN)->           {'ESME_RX_R_APPN',             "ESME_RX_R_APPN",           "ESME Receiver Reject Message"};
err(?ESME_RQUERYFAIL)->          {'ESME_RQUERYFAIL',            "ESME_RQUERYFAIL",          "query_sm request failed"};
err(?ESME_RINVTLVSTREAM)->       {'ESME_RINVTLVSTREAM',         "ESME_RINVTLVSTREAM",       "Error in the optional part of the PDU Body"};
err(?ESME_RTLVNOTALLWD)->        {'ESME_RTLVNOTALLWD',          "ESME_RTLVNOTALLWD",        "TLV not allowed"};
err(?ESME_RINVTLVLEN)->          {'ESME_RINVTLVLEN',            "ESME_RINVTLVLEN",          "Invalid Parameter Length"};
err(?ESME_RMISSINGTLV)->         {'ESME_RMISSINGTLV',           "ESME_RMISSINGTLV",         "Expected TLV missing"};
err(?ESME_RINVTLVVAL)->          {'ESME_RINVTLVVAL',            "ESME_RINVTLVVAL",          "Invalid TLV Value"};
err(?ESME_RDELIVERYFAILURE)->    {'ESME_RDELIVERYFAILURE',      "ESME_RDELIVERYFAILURE",    "Transaction Delivery Failure"};
err(?ESME_RUNKNOWNERR)->         {'ESME_RUNKNOWNERR',           "ESME_RUNKNOWNERR",         "Unknown Error"};
err(?ESME_RSERTYPUNAUTH)->       {'ESME_RSERTYPUNAUTH',         "ESME_RSERTYPUNAUTH",       "ESME Not authorised to use specified service_type"};
err(?ESME_RPROHIBITED)->         {'ESME_RPROHIBITED',           "ESME_RPROHIBITED",         "ESME Prohibited from using specified operation"};
err(?ESME_RSERTYPUNAVAIL)->      {'ESME_RSERTYPUNAVAIL',        "ESME_RSERTYPUNAVAIL",      "Specified service_type is unavailable"};
err(?ESME_RSERTYPDENIED)->       {'ESME_RSERTYPDENIED',         "ESME_RSERTYPDENIED",       "Specified service_type denied"};
err(?ESME_RINVDCS)->             {'ESME_RINVDCS',               "ESME_RINVDCS",             "Invalid Data Coding Scheme"};
err(?ESME_RINVSRCADDRSUBUNIT)->  {'ESME_RINVSRCADDRSUBUNIT',    "ESME_RINVSRCADDRSUBUNIT",  "Source Address Sub unit is invalid"};
err(?ESME_RINVDSTADDRSUBUNIT)->  {'ESME_RINVDSTADDRSUBUNIT',    "ESME_RINVDSTADDRSUBUNIT",  "Destination Address Sub unit is invalid"};
err(?ESME_RINVBCASTFREQINT)->    {'ESME_RINVBCASTFREQINT',      "ESME_RINVBCASTFREQINT",    "Broadcast Frequency Interval is invalid"};
err(?ESME_RINVBCASTALIAS_NAME)-> {'ESME_RINVBCASTALIAS_NAME',   "ESME_RINVBCASTALIAS_NAME", "Invalid Broadcast Alias Name"};
err(?ESME_RINVBCASTAREAFMT)->    {'ESME_RINVBCASTAREAFMT',      "ESME_RINVBCASTAREAFMT",    "Invalid Broadcast Area Format"};
err(?ESME_RINVNUMBCAST_AREAS)->  {'ESME_RINVNUMBCAST_AREAS',    "ESME_RINVNUMBCAST_AREAS",  "Number of Broadcast Areas is invalid"};
err(?ESME_RINVBCASTCNTTYPE)->    {'ESME_RINVBCASTCNTTYPE',      "ESME_RINVBCASTCNTTYPE",    "Invalid Broadcast Content Type"};
err(?ESME_RINVBCASTMSGCLASS)->   {'ESME_RINVBCASTMSGCLASS',     "ESME_RINVBCASTMSGCLASS",   "Broadcast Message Class is invalid"};
err(?ESME_RBCASTFAIL)->          {'ESME_RBCASTFAIL',            "ESME_RBCASTFAIL",          "broadcast_sm operation failed"};
err(?ESME_RBCASTQUERYFAIL)->     {'ESME_RBCASTQUERYFAIL',       "ESME_RBCASTQUERYFAIL",     "query_broadcast_sm failed"};
err(?ESME_RBCASTCANCELFAIL)->    {'ESME_RBCASTCANCELFAIL',      "ESME_RBCASTCANCELFAIL",    "cancel_broadcast_sm failed"};
err(?ESME_RINVBCAST_REP)->       {'ESME_RINVBCAST_REP',         "ESME_RINVBCAST_REP",       "Number of Repeated Broadcasts is invalid"};
err(?ESME_RINVBCASTSRVGRP)->     {'ESME_RINVBCASTSRVGRP',       "ESME_RINVBCASTSRVGRP",     "Broadcast Service Group is invalid"};
err(?ESME_RINVBCASTCHANIND)->    {'ESME_RINVBCASTCHANIND',      "ESME_RINVBCASTCHANIND",    "Broadcast Channel Indicator is invalid"}.
%err(?ESME_RINVOPTPARSTREAM)->    {'ESME_RINVOPTPARSTREAM',      "ESME_RINVOPTPARSTREAM",    "Error in the optional part of the PDU Body"};
%err(?ESME_ROPTPARNOTALLWD)->     {'ESME_ROPTPARNOTALLWD',       "ESME_ROPTPARNOTALLWD",     "Optional Parameter not allowed"};
%err(?ESME_RINVPARLEN)->          {'ESME_RINVPARLEN',            "ESME_RINVPARLEN",          "Invalid Parameter Length"};
%err(?ESME_RMISSINGOPTPARAM)->    {'ESME_RMISSINGOPTPARAM',      "ESME_RMISSINGOPTPARAM",    "Expected Optional Parameter missing"};
%err(?ESME_RINVOPTPARAMVAL)->     {'ESME_RINVOPTPARAMVAL',       "ESME_RINVOPTPARAMVAL",     "Invalid Optional Parameter Value"}.

rec_info(telematics_id) ->
    record_info(fields, telematics_id);
rec_info(callback_num) ->
    record_info(fields, callback_num);
rec_info(its_session_info) ->
    record_info(fields, its_session_info);
rec_info(ms_validity_absolute) ->
    record_info(fields, ms_validity_absolute);
rec_info(callback_num_atag) ->
    record_info(fields, callback_num_atag);
rec_info(Type) -> 
    io:format("Rec info not defined for type : ~p~n", [Type]),
    [].

rec_type(ms_validity) -> ms_validity_absolute;
rec_type(dest_telematics_id) -> telematics_id;
rec_type(source_telematics_id) -> telematics_id;
rec_type(Type) -> Type.

cmdstr(Cmd) when is_integer(Cmd) -> atom_to_binary(cmd(Cmd),utf8).

-compile({inline,[cmd/1]}).
cmd(?COMMAND_ID_UNBIND)                     -> unbind;
cmd(?COMMAND_ID_OUTBIND)                    -> outbind;
cmd(?COMMAND_ID_DATA_SM)                    -> data_sm;
cmd(?COMMAND_ID_QUERY_SM)                   -> query_sm;
cmd(?COMMAND_ID_CANCEL_SM)                  -> cancel_sm;
cmd(?COMMAND_ID_SUBMIT_SM)                  -> submit_sm;
cmd(?COMMAND_ID_REPLACE_SM)                 -> replace_sm;
cmd(?COMMAND_ID_DELIVER_SM)                 -> deliver_sm;
cmd(?COMMAND_ID_UNBIND_RESP)                -> unbind_resp;
cmd(?COMMAND_ID_SUBMIT_MULTI)               -> submit_multi;
cmd(?COMMAND_ID_BROADCAST_SM)               -> broadcast_sm;
cmd(?COMMAND_ID_ENQUIRE_LINK)               -> enquire_link;
cmd(?COMMAND_ID_GENERIC_NACK)               -> generic_nack;
cmd(?COMMAND_ID_DATA_SM_RESP)               -> data_sm_resp;
cmd(?COMMAND_ID_QUERY_SM_RESP)              -> query_sm_resp;
cmd(?COMMAND_ID_BIND_RECEIVER)              -> bind_receiver;
cmd(?COMMAND_ID_CANCEL_SM_RESP)             -> cancel_sm_resp;
cmd(?COMMAND_ID_SUBMIT_SM_RESP)             -> submit_sm_resp;
cmd(?COMMAND_ID_REPLACE_SM_RESP)            -> replace_sm_resp;
cmd(?COMMAND_ID_DELIVER_SM_RESP)            -> deliver_sm_resp;
cmd(?COMMAND_ID_BIND_TRANSCEIVER)           -> bind_transceiver;
cmd(?COMMAND_ID_BIND_TRANSMITTER)           -> bind_transmitter;
cmd(?COMMAND_ID_SUBMIT_MULTI_RESP)          -> submit_multi_resp;
cmd(?COMMAND_ID_BROADCAST_SM_RESP)          -> broadcast_sm_resp;
cmd(?COMMAND_ID_ENQUIRE_LINK_RESP)          -> enquire_link_resp;
cmd(?COMMAND_ID_ALERT_NOTIFICATION)         -> alert_notification;
cmd(?COMMAND_ID_QUERY_BROADCAST_SM)         -> query_broadcast_sm;
cmd(?COMMAND_ID_BIND_RECEIVER_RESP)         -> bind_receiver_resp;
cmd(?COMMAND_ID_CANCEL_BROADCAST_SM)        -> cancel_broadcast_sm;
cmd(?COMMAND_ID_BIND_TRANSCEIVER_RESP)      -> bind_transceiver_resp;
cmd(?COMMAND_ID_BIND_TRANSMITTER_RESP)      -> bind_transmitter_resp;
cmd(?COMMAND_ID_QUERY_BROADCAST_SM_RESP)    -> query_broadcast_sm_resp;
cmd(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP)   -> cancel_broadcast_sm_resp;

cmd(unbind)                     -> ?COMMAND_ID_UNBIND;
cmd(outbind)                    -> ?COMMAND_ID_OUTBIND;
cmd(data_sm)                    -> ?COMMAND_ID_DATA_SM;
cmd(query_sm)                   -> ?COMMAND_ID_QUERY_SM;
cmd(cancel_sm)                  -> ?COMMAND_ID_CANCEL_SM;
cmd(submit_sm)                  -> ?COMMAND_ID_SUBMIT_SM;
cmd(replace_sm)                 -> ?COMMAND_ID_REPLACE_SM;
cmd(deliver_sm)                 -> ?COMMAND_ID_DELIVER_SM;
cmd(unbind_resp)                -> ?COMMAND_ID_UNBIND_RESP;
cmd(submit_multi)               -> ?COMMAND_ID_SUBMIT_MULTI;
cmd(broadcast_sm)               -> ?COMMAND_ID_BROADCAST_SM;
cmd(enquire_link)               -> ?COMMAND_ID_ENQUIRE_LINK;
cmd(generic_nack)               -> ?COMMAND_ID_GENERIC_NACK;
cmd(data_sm_resp)               -> ?COMMAND_ID_DATA_SM_RESP;
cmd(bind_receiver)              -> ?COMMAND_ID_BIND_RECEIVER;
cmd(query_sm_resp)              -> ?COMMAND_ID_QUERY_SM_RESP;
cmd(submit_sm_resp)             -> ?COMMAND_ID_SUBMIT_SM_RESP;
cmd(cancel_sm_resp)             -> ?COMMAND_ID_CANCEL_SM_RESP;
cmd(replace_sm_resp)            -> ?COMMAND_ID_REPLACE_SM_RESP;
cmd(deliver_sm_resp)            -> ?COMMAND_ID_DELIVER_SM_RESP;
cmd(bind_transceiver)           -> ?COMMAND_ID_BIND_TRANSCEIVER;
cmd(bind_transmitter)           -> ?COMMAND_ID_BIND_TRANSMITTER;
cmd(submit_multi_resp)          -> ?COMMAND_ID_SUBMIT_MULTI_RESP;
cmd(broadcast_sm_resp)          -> ?COMMAND_ID_BROADCAST_SM_RESP;
cmd(enquire_link_resp)          -> ?COMMAND_ID_ENQUIRE_LINK_RESP;
cmd(bind_receiver_resp)         -> ?COMMAND_ID_BIND_RECEIVER_RESP;
cmd(alert_notification)         -> ?COMMAND_ID_ALERT_NOTIFICATION;
cmd(query_broadcast_sm)         -> ?COMMAND_ID_QUERY_BROADCAST_SM;
cmd(cancel_broadcast_sm)        -> ?COMMAND_ID_CANCEL_BROADCAST_SM;
cmd(bind_transceiver_resp)      -> ?COMMAND_ID_BIND_TRANSCEIVER_RESP;
cmd(bind_transmitter_resp)      -> ?COMMAND_ID_BIND_TRANSMITTER_RESP;
cmd(query_broadcast_sm_resp)    -> ?COMMAND_ID_QUERY_BROADCAST_SM_RESP;
cmd(cancel_broadcast_sm_resp)   -> ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP;

cmd({Cmd,S,SN,B}) -> {cmd(Cmd),S,SN,B}.

-spec(encode(PDU :: map()) -> {ok, HEX_STRING :: binary()} | {error, binary()}).
encode(PDU) when is_map(PDU) ->
    case pack(json2internal(PDU)) of
        {ok, Bin} ->
            {ok, 
                list_to_binary(string:join([string:pad(integer_to_list(B, 16), 2, leading, $0) || <<B>> <= Bin], " "))};
        {error, _, S, _} ->
            {error, list_to_binary(element(3, err(S)))}
    end;
encode(_) ->
    {error, <<"Input to encode should be map">>}.

-spec(decode(HEX_STRING :: string() | binary()) -> {ok, PDU :: map()} | {error, binary()}).
decode(HexStr) when is_list(HexStr) ->
    decode(list_to_binary(HexStr));
decode(HexBin) when is_binary(HexBin) ->
    ModBin = binary:replace(HexBin, <<" ">>, <<>>, [global]),
    Bin = list_to_binary([binary_to_integer(B, 16) || <<B:2/binary>> <= ModBin]),
    case unpack_map(Bin) of
        {error, _, S, _} ->
            {error, list_to_binary(element(3, err(S)))};
        PDU ->
            {ok, internal2json(PDU)}
    end;    
decode(_) ->
    {error, <<"Input to decode should be Hex String">>}.

%% ===================================================================
%% TESTS
%% ===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TESTS,
[{"bind_transceiver",
  "00 00 00 23 00 00 00 09 00 00 00 00 00 00 00 01 34 31 31 35 00 7A 75 6A 5F "
  "34 31 31 35 00 00 34 00 00 00"},
 {"bind_transmitter",
  "00 00 00 27 00 00 00 02 00 00 00 00 00 00 00 01 34 31 31 35 00 66 64 73 66 "
  "61 6A 66 6B 00 74 65 73 74 00 34 04 08 00"},
 {"bind_receiver",
  "00 00 00 2A 00 00 00 01 00 00 00 00 00 00 00 01 34 31 31 35 00 66 64 67 61 "
  "73 72 67 73 00 74 65 73 74 69 6E 67 00 34 02 09 00"},
 {"submit_sm",
  "00 00 00 44 00 00 00 04 00 00 00 00 00 00 00 01 00 05 00 34 31 30 33 37 00 "
  "00 00 30 37 39 34 36 35 30 31 31 35 00 00 00 00 00 00 11 00 00 00 14 74 65 "
  "73 74 20 73 77 69 73 73 63 6F 6D 20 28 53 4D 50 50 29"},
 {"deliver_sm",
  "00 00 00 44 00 00 00 05 00 00 00 00 00 00 00 01 00 00 00 34 31 37 39 34 34 "
  "34 34 34 34 34 00 00 00 31 32 33 34 35 00 00 00 00 00 00 00 00 00 00 13 68 "
  "69 20 67 6F 74 20 79 6F 75 72 20 6D 65 73 73 61 67 65"},
 {"deliver_sm_resp",
  "00 00 00 11 80 00 00 05 00 00 00 00 00 00 00 01 00"},
 {"submit_sm_optional",
  "00 00 00 51 00 00 00 04 00 00 00 00 00 00 00 01 34 31 31 35 00 01 09 31 39 "
  "32 2E 32 35 34 2E 32 35 34 2E 31 37 00 03 01 31 34 38 2E 32 34 37 2E 31 35 "
  "37 2E 32 35 00 03 00 00 00 00 00 00 00 00 00 02 05 00 01 04 02 04 00 02 00 "
  "03 04 26 00 01 01"},
 {"submit_multi",
  "00 00 00 6D 00 00 00 21 00 00 00 00 00 00 00 01 74 65 73 74 00 01 01 31 32 "
  "2E 35 34 2E 32 36 2E 32 38 00 02 01 00 00 00 01 01 01 31 32 2E 32 34 2E 32 "
  "35 2E 36 33 00 CB 40 03 31 35 30 31 30 35 31 35 31 33 32 35 36 39 39 2B 00 "
  "31 35 30 31 30 35 31 35 31 33 32 35 36 39 39 2B 00 17 00 0A 06 0C 74 65 73 "
  "74 20 6D 65 73 73 61 67 65"},
 {"data_sm",
  "00 00 00 3D 00 00 01 03 00 00 00 00 00 00 00 01 34 31 31 35 00 03 08 31 32 "
  "35 2E 31 32 35 2E 31 32 34 2E 32 34 35 00 01 01 31 32 34 2E 31 34 37 35 2E "
  "32 35 30 2E 31 34 37 00 03 00 00"},
 {"query_sm",
  "00 00 00 24 00 00 00 03 00 00 00 00 00 00 00 01 35 00 01 0E 31 35 34 2E 31 "
  "35 36 2E 31 35 34 2E 31 32 34 00"},
 {"replace_sm",
  "00 00 00 52 00 00 00 07 00 00 00 00 00 00 00 01 35 00 03 0E 31 32 2E 31 35 "
  "33 2E 32 35 2E 34 38 00 31 36 30 31 30 34 31 35 34 34 32 33 36 31 32 2D 00 "
  "31 36 30 31 30 34 31 35 34 34 32 39 36 31 32 2D 00 1A 06 0C 74 65 73 74 20 "
  "6D 65 73 73 61 67 65"},
 {"cancel_sm",
  "00 00 00 32 00 00 00 08 00 00 00 00 00 00 00 01 00 00 01 08 31 34 32 2E 32 "
  "35 2E 39 35 2E 36 38 00 01 0E 31 35 32 2E 31 34 32 2E 31 33 36 2E 37 38 00"}
]).

packunpack_test_() ->
    {inparallel,
     [{T, fun() ->
            Bin = list_to_binary([binary_to_integer(B, 16) || B <- re:split(L, " ")]),
            SMPP = unpack_map(Bin),
            JSON = internal2json(SMPP),
            ?debugFmt("~s~n~p~n~s~n",
                      [T, SMPP, jsx:prettify(jsx:encode(JSON))]),
            {ok, NewBin} = pack(SMPP),
            if Bin /= NewBin ->
                   ?debugFmt("~nExpected : ~p~nGot      : ~p", [Bin, NewBin]);
               true -> ok
            end,
            ?assertEqual(Bin, NewBin)
          end}
      || {T,L} <- ?TESTS]
    }.

-define(PDU(_Id,_Extra), <<"{\"command_id\":",(integer_to_binary(_Id))/binary,
                           ",\"command_status\":0,\"sequence_number\":0",
                           _Extra,"}">>).
-define(PDU(_Id), ?PDU(_Id, "")).
-define(PDU_SYSID(_Id), ?PDU(_Id, ",\"system_id\":\"\"")).
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
                I = json2internal(jsx:decode(J, [return_maps])),
                case pack(I) of
                    {error, _ , Error, _} ->
                        ?assertEqual(ok, err(Error));
                    {ok, Bin} ->
                        ?assertMatch(<<_:32/integer,C:32/integer,_/binary>>, Bin),
                        % 2nd pass (complete PDU support test)
                        J1 = jsx:encode(internal2json(unpack_map(Bin))),
                        json2internal(jsx:decode(J1, [return_maps]))
                end
            end}
        || {T,C,J} <- ?TESTS2]
    }.

encode_decode_test_() ->
    {inparallel,
        [{T,
            fun() ->
                {ok, D} = decode(P),
                ?assertEqual(true, is_map(D)),
                ?assertEqual({ok, D}, decode(re:replace(P,"\s","",[global,{return,list}]))),
                ?assertEqual({ok, D}, decode(re:replace(P,"\s","",[global,{return,binary}]))),
                {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                ?assertEqual(true, is_list(E)),
                ?assertEqual({ok, D}, decode(E))
            end}
            || {T,P} <- ?TESTS] ++
        [{T,
            fun() ->
                I = json2internal(jsx:decode(J, [return_maps])),
                case pack(I) of
                    {error, _ , Error, _} ->
                        ?assertEqual(ok, err(Error));
                    {ok, Bin} ->
                        ?assertMatch(<<_:32/integer,C:32/integer,_/binary>>, Bin),
                        {ok, D} = decode(
                                    <<<<(list_to_binary(
                                            string:right(
                                                integer_to_list(B,16),2,$0)
                                        ))/binary>>||<<B>><=Bin>>),
                        ?assertEqual(true, is_map(D))                
                end
            end}
        || {T,C,J} <- ?TESTS2]
    }.

-endif.
