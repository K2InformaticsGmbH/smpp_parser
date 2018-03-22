-module(smpp).
-include("smpp_globals.hrl").

-export([pack/1, unpack/1, unpack_map/1, unpack/2, json2internal/1,
         internal2json/1, encode/1, decode/1, info/0]).

-export([err/1, cmd/1, cmdstr/1, to_enum/1, from_enum/1]).

-safe([unpack_map/1]).

json2internal(SMPP) when is_map(SMPP) ->
    maps:fold(fun(K,V,M) ->
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

internal2json(SMPP) when is_map(SMPP) -> maps:map(fun internal2json/2, SMPP).
internal2json(tlvs, TLVs)             -> [internal2json(V) || V <- TLVs];
internal2json(_, V) when is_list(V)   -> list_to_binary(V);
internal2json(_, V)                   -> V.

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
    smpp_operation:pack(SMPP);
pack([{_,_}|_] = SMPP) ->
    CmdId = proplists:get_value(command_id, SMPP, '$notfound'),
    Status = proplists:get_value(command_status, SMPP, '$notfound'),
    SeqNum = proplists:get_value(sequence_number, SMPP, '$notfound'),
    if CmdId == '$notfound' orelse Status == '$notfound'
       orelse SeqNum == '$notfound' -> error(badpdu);
        true -> ok
    end,
    Body1 = proplists:delete(command_id, SMPP),
    Body2 = proplists:delete(command_status, Body1),
    Body = proplists:delete(sequence_number, Body2),
    pack({CmdId, Status, SeqNum, Body}).

unpack(Bin) -> unpack(Bin, []).
unpack_map(Bin) -> unpack(Bin, [return_maps]).
unpack(Bin, Opts) ->
    case smpp_operation:unpack(Bin) of
        {error, _, S, _} = Error ->
            io:format("Unpack error ~p~n", [err(S)]),
            Error;
        {ok, {CmdId, Status, SeqNum, Body}} ->
            case lists:member(return_maps, Opts) of
                true ->
                    SMPPMap = lists:foldl(fun list_to_map/2, #{}, Body),
                    SMPPMap#{command_id => CmdId, command_status => Status, sequence_number => SeqNum};
                _ ->
                    Hd = [{command_id, CmdId}, {command_status, Status}, {sequence_number, SeqNum}],
                    Hd ++ lists:foldl(fun list_to_pl/2, [], Body)
            end
    end.

list_to_map({tlvs, TLVs}, Acc) ->
    TLVMaps = [#{tag => T, len => L, val => V} || {T, L, V} <- TLVs],
    Acc#{tlvs => TLVMaps ++ maps:get(tlvs, Acc, [])};
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
    
list_to_pl({tlvs, TLVs}, Acc) ->
    [{tlvs, TLVs} | Acc];
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

map_to_rec(tlvs, Map) when is_map(Map) ->
    #{tag := T, len := L, val := V} = Map,
    {T, L, V};
map_to_rec(Type, Map) when is_map(Map) ->
    Rec = rec_type(Type),
    list_to_tuple([Rec | [maps:get(K, Map) || K <- rec_info(Rec)]]).

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
rec_info(network_error_code) ->
    record_info(fields, network_error_code);
rec_info(Type) ->
    io:format("~p:~p:~p unknown ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Type]),
    [].

rec_type(ms_validity) -> ms_validity_absolute;
rec_type(dest_telematics_id) -> telematics_id;
rec_type(source_telematics_id) -> telematics_id;
rec_type(Type) -> Type.

cmdstr(Cmd) when is_integer(Cmd) -> atom_to_binary(cmd(Cmd),utf8).

cmdval(Cmd) when is_binary(Cmd) -> cmd(binary_to_existing_atom(Cmd,utf8));
cmdval(Cmd) -> Cmd.

statusstr(Status) when is_integer(Status) ->
    {_, StatusStr, _} = err(Status),
    list_to_binary(StatusStr).

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
    CmdLen = byte_size(ModBin),
    Bin = list_to_binary([binary_to_integer(B, 16) || <<B:2/binary>> <= ModBin]),
    case unpack_map(Bin) of
        {error, _, S, _} ->
            {error, list_to_binary(element(3, err(S)))};
        PDU ->
            {ok, internal2json(PDU#{command_length => CmdLen})}
    end;
decode(_) ->
    {error, <<"Input to decode should be Hex String">>}.

to_enum(SMPP) when is_map(SMPP) ->
    maps:fold(
        fun(K, V, M) ->
            M#{K => to_enum(K, V)}
        end, #{}, SMPP).
to_enum(K, V) when K == <<"command_id">>;       K == command_id         -> cmdstr(V);
to_enum(K, V) when K == <<"command_status">>;   K == command_status     -> statusstr(V);
to_enum(K, V) when K == <<"addr_ton">>;         K == addr_ton           -> ton(V);
to_enum(K, V) when K == <<"source_addr_ton">>;  K == source_addr_ton    -> ton(V);
to_enum(K, V) when K == <<"dest_addr_ton">>;    K == dest_addr_ton      -> ton(V);
to_enum(K, V) when K == <<"esme_addr_ton">>;    K == esme_addr_ton      -> ton(V);
to_enum(K, V) when K == <<"addr_npi">>;         K == addr_npi           -> npi(V);
to_enum(K, V) when K == <<"source_addr_npi">>;  K == source_addr_npi    -> npi(V);
to_enum(K, V) when K == <<"dest_addr_npi">>;    K == dest_addr_npi      -> npi(V);
to_enum(K, V) when K == <<"esme_addr_npi">>;    K == esme_addr_npi      -> npi(V);
to_enum(K, V) when K == <<"data_coding">>;      K == data_coding        -> enc(V);
to_enum(K, V) when K == <<"message_state">>;    K == message_state      -> msgstate(V);
to_enum(_, V) -> V.

from_enum(SMPP) when is_map(SMPP) ->
    maps:fold(
        fun(K, V, M) ->
            M#{K => from_enum(K, V)}
        end, #{}, SMPP).
from_enum(K, V) when K == <<"command_id">>;       K == command_id       -> cmdval(V);
from_enum(K, V) when (K == <<"command_status">> orelse K == command_status) andalso is_integer(V) -> V;
from_enum(K, V) when K == <<"command_status">>;   K == command_status   -> err(V);
from_enum(K, V) when K == <<"addr_ton">>;         K == addr_ton         -> ton(V);
from_enum(K, V) when K == <<"source_addr_ton">>;  K == source_addr_ton  -> ton(V);
from_enum(K, V) when K == <<"dest_addr_ton">>;    K == dest_addr_ton    -> ton(V);
from_enum(K, V) when K == <<"esme_addr_ton">>;    K == esme_addr_ton    -> ton(V);
from_enum(K, V) when K == <<"addr_npi">>;         K == addr_npi         -> npi(V);
from_enum(K, V) when K == <<"source_addr_npi">>;  K == source_addr_npi  -> npi(V);
from_enum(K, V) when K == <<"dest_addr_npi">>;    K == dest_addr_npi    -> npi(V);
from_enum(K, V) when K == <<"esme_addr_npi">>;    K == esme_addr_npi    -> npi(V);
from_enum(K, V) when K == <<"data_coding">>;      K == data_coding      -> enc(V);
from_enum(K, V) when K == <<"message_state">>;    K == message_state    -> msgstate(V);
from_enum(_, V) -> V.

%% ===================================================================
%% Mapping functions
%% ===================================================================

ton(?TON_UNKNOWN)               -> <<"Unknown">>;
ton(?TON_NATIONAL)              -> <<"National">>;
ton(?TON_ABBREVIATED)           -> <<"Abbreviated">>;
ton(?TON_ALPHANUMERIC)          -> <<"Alphanumeric">>;
ton(?TON_INTERNATIONAL)         -> <<"International">>;
ton(?TON_NETWORK_SPECIFIC)      -> <<"Network Specific">>;
ton(?TON_SUBSCRIBER_NUMBER)     -> <<"Subscriber Number">>;
ton(Ton) when is_integer(Ton)   -> integer_to_binary(Ton);
ton(<<"Unknown">>)              -> ?TON_UNKNOWN;
ton(<<"National">>)             -> ?TON_NATIONAL;
ton(<<"Abbreviated">>)          -> ?TON_ABBREVIATED;
ton(<<"Alphanumeric">>)         -> ?TON_ALPHANUMERIC;
ton(<<"International">>)        -> ?TON_INTERNATIONAL;
ton(<<"Network Specific">>)     -> ?TON_NETWORK_SPECIFIC;
ton(<<"Subscriber Number">>)    -> ?TON_SUBSCRIBER_NUMBER;
ton(Ton) when is_binary(Ton)    -> binary_to_integer(Ton).

npi(?NPI_ERMES)                 -> <<"ERMES">>;
npi(?NPI_UNKNOWN)               -> <<"Unknown">>;
npi(?NPI_PRIVATE)               -> <<"Private">>;
npi(?NPI_NATIONAL)              -> <<"National">>;
npi(?NPI_DATA)                  -> <<"Data (X.121)">>;
npi(?NPI_TELEX)                 -> <<"Telex (F.69)">>;
npi(?NPI_INTERNET)              -> <<"Internet (IP)">>;
npi(?NPI_WAP_CLIENT_ID)         -> <<"WAP Client Id">>;
npi(?NPI_ISDN)                  -> <<"ISDN (E163/E164)">>;
npi(?NPI_LAND_MOBILE)           -> <<"Land Mobile (E.212)">>;
npi(Npi) when is_integer(Npi)   -> integer_to_binary(Npi);
npi(<<"ISDN (E163/E164)">>)     -> ?NPI_ISDN;
npi(<<"Data (X.121)">>)         -> ?NPI_DATA;
npi(<<"Telex (F.69)">>)         -> ?NPI_TELEX;
npi(<<"ERMES">>)                -> ?NPI_ERMES;
npi(<<"Private">>)              -> ?NPI_PRIVATE;
npi(<<"Unknown">>)              -> ?NPI_UNKNOWN;
npi(<<"National">>)             -> ?NPI_NATIONAL;
npi(<<"Internet (IP)">>)        -> ?NPI_INTERNET;
npi(<<"Land Mobile (E.212)">>)  -> ?NPI_LAND_MOBILE;
npi(<<"WAP Client Id">>)        -> ?NPI_WAP_CLIENT_ID;
npi(Npi) when is_binary(Npi)    -> binary_to_integer(Npi).

enc(?ENCODING_SCHEME_KS_C_5601)                 -> <<"KS C 5601">>;
enc(?ENCODING_SCHEME_MC_SPECIFIC)               -> <<"MC Specific">>;
enc(?ENCODING_SCHEME_JIS)                       -> <<"JIS (X 0208-1990)">>;
enc(?ENCODING_SCHEME_PICTOGRAM)                 -> <<"Pictogram Encoding">>;
enc(?ENCODING_SCHEME_UCS2)                      -> <<"UCS2 (ISO/IEC-10646)">>;
enc(?ENCODING_SCHEME_LATIN_1)                   -> <<"Latin 1 (ISO-8859-1)">>;
enc(?ENCODING_SCHEME_CYRILLIC)                  -> <<"Cyrillic (ISO-8859-5)">>;
enc(?ENCODING_SCHEME_ISO_2022_JP)               -> <<"ISO-2022-JP (Music Codes)">>;
enc(?ENCODING_SCHEME_LATIN_HEBREW)              -> <<"Latin/Hebrew (ISO-8859-8)">>;
enc(?ENCODING_SCHEME_BINARY)                    -> <<"Octet unspecified (8-bit binary)">>;
enc(?ENCODING_SCHEME_OCTET)                     -> <<"Octet unspecified (8-bit binary)">>;
enc(?ENCODING_SCHEME_KANJI_JIS)                 -> <<"Extended Kanji JIS (X 0212-1990)">>;
enc(?ENCODING_SCHEME_IA5_ASCII)                 -> <<"IA5 (CCITT T.50)/ASCII (ANSI X3.4)">>;
enc(E) when is_integer(E)                       -> integer_to_binary(E);
enc(<<"JIS (X 0208-1990)">>)                    -> ?ENCODING_SCHEME_JIS;
enc(<<"UCS2 (ISO/IEC-10646)">>)                 -> ?ENCODING_SCHEME_UCS2;
enc(<<"Octet unspecified (8-bit octect)">>)     -> ?ENCODING_SCHEME_OCTET;
enc(<<"Octet unspecified (8-bit binary)">>)     -> ?ENCODING_SCHEME_BINARY;
enc(<<"Latin 1 (ISO-8859-1)">>)                 -> ?ENCODING_SCHEME_LATIN_1;
enc(<<"Cyrillic (ISO-8859-5)">>)                -> ?ENCODING_SCHEME_CYRILLIC;
enc(<<"Extended Kanji JIS (X 0212-1990)">>)     -> ?ENCODING_SCHEME_KANJI_JIS;
enc(<<"IA5 (CCITT T.50)/ASCII (ANSI X3.4)">>)   -> ?ENCODING_SCHEME_IA5_ASCII;
enc(<<"Pictogram Encoding">>)                   -> ?ENCODING_SCHEME_PICTOGRAM;
enc(<<"KS C 5601">>)                            -> ?ENCODING_SCHEME_KS_C_5601;
enc(<<"ISO-2022-JP (Music Codes)">>)            -> ?ENCODING_SCHEME_ISO_2022_JP;
enc(<<"MC Specific">>)                          -> ?ENCODING_SCHEME_MC_SPECIFIC;
enc(<<"Latin/Hebrew (ISO-8859-8)">>)            -> ?ENCODING_SCHEME_LATIN_HEBREW;
enc(E) when is_binary(E)                        -> binary_to_integer(E).

msgstate(?MESSAGE_STATE_ENROUTE)        -> <<"ENROUTE">>;
msgstate(?MESSAGE_STATE_EXPIRED)        -> <<"EXPIRED">>;
msgstate(?MESSAGE_STATE_DELETED)        -> <<"DELETED">>;
msgstate(?MESSAGE_STATE_UNKNOWN)        -> <<"UNKNOWN">>;
msgstate(?MESSAGE_STATE_SKIPPED)        -> <<"SKIPPED">>;
msgstate(?MESSAGE_STATE_REJECTED)       -> <<"REJECTED">>;
msgstate(?MESSAGE_STATE_ACCEPTED)       -> <<"ACCEPTED">>;
msgstate(?MESSAGE_STATE_SCHEDULED)      -> <<"SCHEDULED">>;
msgstate(?MESSAGE_STATE_DELIVERED)      -> <<"DELIVERED">>;
msgstate(?MESSAGE_STATE_UNDELIVERABLE)  -> <<"UNDELIVERABLE">>;
msgstate(E) when is_integer(E)          -> integer_to_binary(E);
msgstate(<<"ENROUTE">>)                 -> ?MESSAGE_STATE_ENROUTE;
msgstate(<<"EXPIRED">>)                 -> ?MESSAGE_STATE_EXPIRED;
msgstate(<<"DELETED">>)                 -> ?MESSAGE_STATE_DELETED;
msgstate(<<"UNKNOWN">>)                 -> ?MESSAGE_STATE_UNKNOWN;
msgstate(<<"SKIPPED">>)                 -> ?MESSAGE_STATE_SKIPPED;
msgstate(<<"REJECTED">>)                -> ?MESSAGE_STATE_REJECTED;
msgstate(<<"ACCEPTED">>)                -> ?MESSAGE_STATE_ACCEPTED;
msgstate(<<"SCHEDULED">>)               -> ?MESSAGE_STATE_SCHEDULED;
msgstate(<<"DELIVERED">>)               -> ?MESSAGE_STATE_DELIVERED;
msgstate(<<"UNDELIVERABLE">>)           -> ?MESSAGE_STATE_UNDELIVERABLE;
msgstate(E) when is_binary(E)           -> binary_to_integer(E).

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

b2a(<<"len">>) -> len;
b2a(<<"tag">>) -> tag;
b2a(<<"val">>) -> val;
b2a(<<"tlvs">>) -> tlvs;
b2a(<<"password">>) -> password;
b2a(<<"addr_npi">>) -> addr_npi;
b2a(<<"addr_ton">>) -> addr_ton;
b2a(<<"esme_addr">>) -> esme_addr;
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
b2a(<<"dest_address">>) -> dest_address;
b2a(<<"service_type">>) -> service_type;
b2a(<<"dest_addr_npi">>) -> dest_addr_npi;
b2a(<<"dest_addr_ton">>) -> dest_addr_ton;
b2a(<<"esme_addr_npi">>) -> esme_addr_npi;
b2a(<<"esme_addr_ton">>) -> esme_addr_ton;
b2a(<<"short_message">>) -> short_message;
b2a(<<"message_state">>) -> message_state;
b2a(<<"address_range">>) -> address_range;
b2a(<<"priority_flag">>) -> priority_flag;
b2a(<<"unsuccess_sme">>) -> unsuccess_sme;
b2a(<<"command_status">>) -> command_status;
b2a(<<"command_length">>) -> command_length;
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
b2a(<<"user_response_code">>) -> user_response_code;
b2a(<<"registered_delivery">>) -> registered_delivery;
b2a(<<"sc_interface_version">>) -> sc_interface_version;
b2a(<<"more_messages_to_send">>) -> more_messages_to_send;
b2a(<<"user_message_reference">>) -> user_message_reference;
b2a(<<"schedule_delivery_time">>) -> schedule_delivery_time;
b2a(<<"replace_if_present_flag">>) -> replace_if_present_flag;
b2a(Field) when is_atom(Field) -> Field.

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
err(?ESME_RINVBCASTCHANIND)->    {'ESME_RINVBCASTCHANIND',      "ESME_RINVBCASTCHANIND",    "Broadcast Channel Indicator is invalid"};
%err(?ESME_RINVOPTPARSTREAM)->    {'ESME_RINVOPTPARSTREAM',      "ESME_RINVOPTPARSTREAM",    "Error in the optional part of the PDU Body"};
%err(?ESME_ROPTPARNOTALLWD)->     {'ESME_ROPTPARNOTALLWD',       "ESME_ROPTPARNOTALLWD",     "Optional Parameter not allowed"};
%err(?ESME_RINVPARLEN)->          {'ESME_RINVPARLEN',            "ESME_RINVPARLEN",          "Invalid Parameter Length"};
%err(?ESME_RMISSINGOPTPARAM)->    {'ESME_RMISSINGOPTPARAM',      "ESME_RMISSINGOPTPARAM",    "Expected Optional Parameter missing"};
%err(?ESME_RINVOPTPARAMVAL)->     {'ESME_RINVOPTPARAMVAL',       "ESME_RINVOPTPARAMVAL",     "Invalid Optional Parameter Value"}.
err(<<"ESME_ROK">>)                 -> ?ESME_ROK;
err(<<"ESME_RINVMSGLEN">>)          -> ?ESME_RINVMSGLEN;
err(<<"ESME_RINVCMDLEN">>)          -> ?ESME_RINVCMDLEN;
err(<<"ESME_RINVCMDID">>)           -> ?ESME_RINVCMDID;
err(<<"ESME_RINVBNDSTS">>)          -> ?ESME_RINVBNDSTS;
err(<<"ESME_RALYBND">>)             -> ?ESME_RALYBND;
err(<<"ESME_RINVPRTFLG">>)          -> ?ESME_RINVPRTFLG;
err(<<"ESME_RINVREGDLVFLG">>)       -> ?ESME_RINVREGDLVFLG;
err(<<"ESME_RSYSERR">>)             -> ?ESME_RSYSERR;
err(<<"ESME_RINVSRCADR">>)          -> ?ESME_RINVSRCADR;
err(<<"ESME_RINVDSTADR">>)          -> ?ESME_RINVDSTADR;
err(<<"ESME_RINVMSGID">>)           -> ?ESME_RINVMSGID;
err(<<"ESME_RBINDFAIL">>)           -> ?ESME_RBINDFAIL;
err(<<"ESME_RINVPASWD">>)           -> ?ESME_RINVPASWD;
err(<<"ESME_RINVSYSID">>)           -> ?ESME_RINVSYSID;
err(<<"ESME_RCANCELFAIL">>)         -> ?ESME_RCANCELFAIL;
err(<<"ESME_RREPLACEFAIL">>)        -> ?ESME_RREPLACEFAIL;
err(<<"ESME_RMSGQFUL">>)            -> ?ESME_RMSGQFUL;
err(<<"ESME_RINVSERTYP">>)          -> ?ESME_RINVSERTYP;
err(<<"ESME_RINVNUMDESTS">>)        -> ?ESME_RINVNUMDESTS;
err(<<"ESME_RINVDLNAME">>)          -> ?ESME_RINVDLNAME;
err(<<"ESME_RINVDESTFLAG">>)        -> ?ESME_RINVDESTFLAG;
err(<<"ESME_RINVSUBREP">>)          -> ?ESME_RINVSUBREP;
err(<<"ESME_RINVESMCLASS">>)        -> ?ESME_RINVESMCLASS;
err(<<"ESME_RCNTSUBDL">>)           -> ?ESME_RCNTSUBDL;
err(<<"ESME_RSUBMITFAIL">>)         -> ?ESME_RSUBMITFAIL;
err(<<"ESME_RINVSRCTON">>)          -> ?ESME_RINVSRCTON;
err(<<"ESME_RINVSRCNPI">>)          -> ?ESME_RINVSRCNPI;
err(<<"ESME_RINVDSTTON">>)          -> ?ESME_RINVDSTTON;
err(<<"ESME_RINVDSTNPI">>)          -> ?ESME_RINVDSTNPI;
err(<<"ESME_RINVSYSTYP">>)          -> ?ESME_RINVSYSTYP;
err(<<"ESME_RINVREPFLAG">>)         -> ?ESME_RINVREPFLAG;
err(<<"ESME_RINVNUMMSGS">>)         -> ?ESME_RINVNUMMSGS;
err(<<"ESME_RTHROTTLED">>)          -> ?ESME_RTHROTTLED;
err(<<"ESME_RINVSCHED">>)           -> ?ESME_RINVSCHED;
err(<<"ESME_RINVEXPIRY">>)          -> ?ESME_RINVEXPIRY;
err(<<"ESME_RINVDFTMSGID">>)        -> ?ESME_RINVDFTMSGID;
err(<<"ESME_RX_T_APPN">>)           -> ?ESME_RX_T_APPN;
err(<<"ESME_RX_P_APPN">>)           -> ?ESME_RX_P_APPN;
err(<<"ESME_RX_R_APPN">>)           -> ?ESME_RX_R_APPN;
err(<<"ESME_RQUERYFAIL">>)          -> ?ESME_RQUERYFAIL;
err(<<"ESME_RINVTLVSTREAM">>)       -> ?ESME_RINVTLVSTREAM;
err(<<"ESME_RTLVNOTALLWD">>)        -> ?ESME_RTLVNOTALLWD;
err(<<"ESME_RINVTLVLEN">>)          -> ?ESME_RINVTLVLEN;
err(<<"ESME_RMISSINGTLV">>)         -> ?ESME_RMISSINGTLV;
err(<<"ESME_RINVTLVVAL">>)          -> ?ESME_RINVTLVVAL;
err(<<"ESME_RDELIVERYFAILURE">>)    -> ?ESME_RDELIVERYFAILURE;
err(<<"ESME_RUNKNOWNERR">>)         -> ?ESME_RUNKNOWNERR;
err(<<"ESME_RSERTYPUNAUTH">>)       -> ?ESME_RSERTYPUNAUTH;
err(<<"ESME_RPROHIBITED">>)         -> ?ESME_RPROHIBITED;
err(<<"ESME_RSERTYPUNAVAIL">>)      -> ?ESME_RSERTYPUNAVAIL;
err(<<"ESME_RSERTYPDENIED">>)       -> ?ESME_RSERTYPDENIED;
err(<<"ESME_RINVDCS">>)             -> ?ESME_RINVDCS;
err(<<"ESME_RINVSRCADDRSUBUNIT">>)  -> ?ESME_RINVSRCADDRSUBUNIT;
err(<<"ESME_RINVDSTADDRSUBUNIT">>)  -> ?ESME_RINVDSTADDRSUBUNIT;
err(<<"ESME_RINVBCASTFREQINT">>)    -> ?ESME_RINVBCASTFREQINT;
err(<<"ESME_RINVBCASTALIAS_NAME">>) -> ?ESME_RINVBCASTALIAS_NAME;
err(<<"ESME_RINVBCASTAREAFMT">>)    -> ?ESME_RINVBCASTAREAFMT;
err(<<"ESME_RINVNUMBCAST_AREAS">>)  -> ?ESME_RINVNUMBCAST_AREAS;
err(<<"ESME_RINVBCASTCNTTYPE">>)    -> ?ESME_RINVBCASTCNTTYPE;
err(<<"ESME_RINVBCASTMSGCLASS">>)   -> ?ESME_RINVBCASTMSGCLASS;
err(<<"ESME_RBCASTFAIL">>)          -> ?ESME_RBCASTFAIL;
err(<<"ESME_RBCASTQUERYFAIL">>)     -> ?ESME_RBCASTQUERYFAIL;
err(<<"ESME_RBCASTCANCELFAIL">>)    -> ?ESME_RBCASTCANCELFAIL;
err(<<"ESME_RINVBCAST_REP">>)       -> ?ESME_RINVBCAST_REP;
err(<<"ESME_RINVBCASTSRVGRP">>)     -> ?ESME_RINVBCASTSRVGRP;
err(<<"ESME_RINVBCASTCHANIND">>)    -> ?ESME_RINVBCASTCHANIND;
err(<<"ESME_RINVOPTPARSTREAM">>)    -> ?ESME_RINVOPTPARSTREAM;
err(<<"ESME_ROPTPARNOTALLWD">>)     -> ?ESME_ROPTPARNOTALLWD;
err(<<"ESME_RINVPARLEN">>)          -> ?ESME_RINVPARLEN;
err(<<"ESME_RMISSINGOPTPARAM">>)    -> ?ESME_RMISSINGOPTPARAM;
err(<<"ESME_RINVOPTPARAMVAL">>)     -> ?ESME_RINVOPTPARAMVAL.

-define(BASE(_Id),
        #{command_id => _Id, command_status => 0, sequence_number => 0}).
-define(M_SYS_ID(_Id), ?BASE(_Id)#{system_id => ""}).
-define(M_DST_ADDR(_Id), ?BASE(_Id)#{destination_addr => ""}).
info() ->
    #{templates =>
        #{unbind                    => ?BASE(cmd(unbind)),
          query_sm                  => ?BASE(cmd(query_sm)),
          replace_sm                => ?BASE(cmd(replace_sm)),
          outbind                   => ?M_SYS_ID(cmd(outbind)),
          enquire_link              => ?BASE(cmd(enquire_link)),
          data_sm                   => ?M_DST_ADDR(cmd(data_sm)),
          cancel_sm                 => ?M_DST_ADDR(cmd(cancel_sm)),
          submit_sm                 => ?M_DST_ADDR(cmd(submit_sm)),
          deliver_sm                => ?M_DST_ADDR(cmd(deliver_sm)),
          bind_receiver             => ?M_SYS_ID(cmd(bind_receiver)),
          alert_notification        => ?BASE(cmd(alert_notification)),
          query_broadcast_sm        => ?BASE(cmd(query_broadcast_sm)),
          cancel_broadcast_sm       => ?BASE(cmd(cancel_broadcast_sm)),   
          bind_transmitter          => ?M_SYS_ID(cmd(bind_transmitter)),
          bind_transceiver          => ?M_SYS_ID(cmd(bind_transceiver)),
          submit_multi              => ?BASE(cmd(submit_multi))
                                                    #{dest_address => []},

          unbind_resp               => ?BASE(cmd(unbind_resp)),
          data_sm_resp              => ?BASE(cmd(data_sm_resp)),
          generic_nack              => ?BASE(cmd(generic_nack)),
          query_sm_resp             => ?BASE(cmd(query_sm_resp)),
          submit_sm_resp            => ?BASE(cmd(submit_sm_resp)),
          cancel_sm_resp            => ?BASE(cmd(cancel_sm_resp)),
          deliver_sm_resp           => ?BASE(cmd(deliver_sm_resp)),
          replace_sm_resp           => ?BASE(cmd(replace_sm_resp)),
          broadcast_sm_resp         => ?BASE(cmd(broadcast_sm_resp)),
          enquire_link_resp         => ?BASE(cmd(enquire_link_resp)),
          bind_receiver_resp        => ?M_SYS_ID(cmd(bind_receiver_resp)),
          cancel_broadcast_sm_resp  => ?BASE(cmd(cancel_broadcast_sm_resp)),
          bind_transceiver_resp     => ?M_SYS_ID(cmd(bind_transceiver_resp)),
          bind_transmitter_resp     => ?M_SYS_ID(cmd(bind_transmitter_resp)),
          submit_multi_resp         => ?BASE(cmd(submit_multi_resp))
                                                       #{unsuccess_sme => []}},
      schema => schema()}.

-include("smpp_pdu.hrl").

-define(BINLIST(__R,__T,__F),
        [atom_to_binary(N, utf8) || #__R{name = N} <- ?__T#pdu.__F]).
-define(SPEC(__T),
    #{props => ?BINLIST(standard,__T,std_types),
      tlvs  => ?BINLIST(tlv,__T,tlv_types),
      opts  => [#{tag => 16#1400, len => 0, val => <<>>}
                | ?BINLIST(tlv,__T,opt_types)]}
).
schema() ->
    #{bind_transmitter         => ?SPEC(BIND_TRANSMITTER),
      bind_transmitter_resp    => ?SPEC(BIND_TRANSMITTER_RESP),
      bind_receiver            => ?SPEC(BIND_RECEIVER),
      bind_receiver_resp       => ?SPEC(BIND_RECEIVER_RESP),
      bind_transceiver         => ?SPEC(BIND_TRANSCEIVER),
      bind_transceiver_resp    => ?SPEC(BIND_TRANSCEIVER_RESP),
      outbind                  => ?SPEC(OUTBIND),
      unbind                   => ?SPEC(UNBIND),
      unbind_resp              => ?SPEC(UNBIND_RESP),
      enquire_link             => ?SPEC(ENQUIRE_LINK),
      enquire_link_resp        => ?SPEC(ENQUIRE_LINK_RESP),
      alert_notification       => ?SPEC(ALERT_NOTIFICATION),
      generic_nack             => ?SPEC(GENERIC_NACK),
      submit_sm                => ?SPEC(SUBMIT_SM),
      submit_sm_resp           => ?SPEC(SUBMIT_SM_RESP),
      data_sm                  => ?SPEC(DATA_SM),
      data_sm_resp             => ?SPEC(DATA_SM_RESP),
      submit_multi             => ?SPEC(SUBMIT_MULTI),
      submit_multi_resp        => ?SPEC(SUBMIT_MULTI_RESP),
      deliver_sm               => ?SPEC(DELIVER_SM),
      deliver_sm_resp          => ?SPEC(DELIVER_SM_RESP),
      broadcast_sm             => ?SPEC(BROADCAST_SM),
      broadcast_sm_resp        => ?SPEC(BROADCAST_SM_RESP),
      cancel_sm                => ?SPEC(CANCEL_SM),
      cancel_sm_resp           => ?SPEC(CANCEL_SM_RESP),
      query_sm                 => ?SPEC(QUERY_SM),
      query_sm_resp            => ?SPEC(QUERY_SM_RESP),
      replace_sm               => ?SPEC(REPLACE_SM),
      replace_sm_resp          => ?SPEC(REPLACE_SM_RESP),
      query_broadcast_sm       => ?SPEC(QUERY_BROADCAST_SM),
      query_broadcast_sm_resp  => ?SPEC(QUERY_BROADCAST_SM_RESP),
      cancel_broadcast_sm      => ?SPEC(CANCEL_BROADCAST_SM),
      cancel_broadcast_sm_resp => ?SPEC(CANCEL_BROADCAST_SM_RESP)}.

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
                ?assertEqual(true, is_binary(E)),
                ?assertEqual({ok, D}, decode(E))
            end}
        || {T,P} <- ?TESTS]
    }.

encode_decode_1_test_() ->
    {inparallel,
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

enum_test_() ->
    {inparallel,
        [{T,
            fun() ->
                I = json2internal(jsx:decode(J, [return_maps])),
                {ok, Bin} = pack(I),
                {ok, #{} = D} = decode(
                    <<<<(list_to_binary(
                        string:right(integer_to_list(B,16),2,$0)))/binary>>
                    || <<B>> <= Bin>>),
                S = jsx:decode(jsx:encode(D), [return_maps]),
                #{<<"command_id">> := CmdId,
                  <<"command_status">> := CommandStatus} = S1 = to_enum(S),
                ?assertEqual(true, is_binary(CmdId)),
                ?assertEqual(true, is_binary(CommandStatus)),
                ?assertEqual(S, from_enum(S1))
            end}
        || {T,_,J} <- ?TESTS2]
    }.


templates_test_() ->
    #{templates := Templates} = info(),
    {inparallel,
        maps:fold(
            fun(T, Pdu, Acc) ->
                [{atom_to_list(T),
                  fun() ->
                    case pack(Pdu) of
                        {error, _ , Error, _} ->
                            ?assertEqual(ok, err(Error));
                        {ok, Bin} ->
                            Pdu2 = unpack_map(Bin),
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
                 {ok, #{tlvs := Tlvs} = D0} = decode(I),
                 D = D0#{tlvs => lists:usort(Tlvs)},
                 ?assertEqual(O, maps:without(?IGNORE_FIELDS, D)),
                 {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                 ?assertEqual(true, is_binary(E)),
                 {ok, #{tlvs := Tlvs1} = D00} = decode(E),
                 D1 = D00#{tlvs => lists:usort(Tlvs1)},
                 ?assertEqual(O, maps:without(?IGNORE_FIELDS, D1))
            end
         } || {T,I,O} <-
            [
                {"submit_sm",
                 "00 00 00 31 00 00 00 04 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
                 "02 04 00 01 01 " % user_message_reference
                 "14 00 00 01 02 "
                 "14 01 00 02 03 04",
                 #{command_id => 4,
                   user_message_reference => 16#01,
                   tlvs => [#{tag => 16#1400, len => 1, val => <<16#02>>},
                            #{tag => 16#1401, len => 2, val => <<16#03, 16#04>>}]
                  }
                },
                {"submit_sm-1",
                 "00 00 00 31 00 00 00 04 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "
                 "14 01 00 02 03 04"
                 "14 00 00 01 02 "
                 "02 04 00 01 01 ", % user_message_reference
                 #{command_id => 4,
                   user_message_reference => 16#01,
                   tlvs => [#{tag => 16#1400, len => 1, val => <<16#02>>},
                            #{tag => 16#1401, len => 2, val => <<16#03, 16#04>>}]
                  }
                }
            ]
        ]
    }.

schema_test() ->
    #{schema := Schema} = info(),
    ?assertEqual(true, is_binary(jsx:encode(Schema))).

-endif.