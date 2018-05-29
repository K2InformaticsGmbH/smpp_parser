-module(smpp).
-include("smpp_globals.hrl").

-export([pack/1, unpack/1, unpack_map/1, unpack/2, json2internal/1,
         internal2json/1, encode/1, decode/1, decode_bin/1, info/0]).

-export([err/1, cmd/1, cmdstr/1, to_enum/1, from_enum/1, list_to_map/2]).

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

internal2json({error, _, _, _} = Err) -> error(err(Err));
internal2json(SMPP) when is_map(SMPP) -> maps:map(fun internal2json/2, SMPP).
internal2json(tlvs, TLVs)             -> [internal2json(V) || V <- TLVs];
internal2json(_, [M|_] = V) when is_map(M) -> [internal2json(I) || I <- V];
internal2json(broadcast_area_success, V) -> V;
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
                    SMPPMap#{command_id => CmdId, command_status => Status,
                             sequence_number => SeqNum,
                             command_length => byte_size(Bin)};
                _ ->
                    Hd = [{command_id, CmdId}, {command_status, Status},
                          {sequence_number, SeqNum},
                          {command_length, byte_size(Bin)}],
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
map_to_rec(dest_address, #{dl_name := _} = DA) ->
    Rec = rec_type(dest_address_dl),
    list_to_tuple([Rec | [maps:get(K, DA) || K <- rec_info(Rec)]]);
map_to_rec(dest_address, DA) ->
    Rec = rec_type(dest_address_sme),
    list_to_tuple([Rec | [maps:get(K, DA) || K <- rec_info(Rec)]]);
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
rec_info(unsuccess_sme) ->
    record_info(fields, unsuccess_sme);
rec_info(broadcast_area) ->
    record_info(fields, broadcast_area);
rec_info(broadcast_frequency_interval) ->
    record_info(fields, broadcast_frequency_interval);
rec_info(broadcast_content_type) ->
    record_info(fields, broadcast_content_type);
rec_info(broadcast_area_identifier) ->
    record_info(fields, broadcast_area);
rec_info(dest_address_sme) ->
    record_info(fields, dest_address_sme);
rec_info(dest_address_dl) ->
    record_info(fields, dest_address_dl);
rec_info(Type) ->
    io:format("~p:~p:~p unknown ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Type]),
    [].

rec_type(ms_validity) -> ms_validity_absolute;
rec_type(dest_telematics_id) -> telematics_id;
rec_type(source_telematics_id) -> telematics_id;
rec_type(broadcast_area_identifier) -> broadcast_area;
rec_type(Type) -> Type.

cmdstr(Cmd) when is_integer(Cmd) -> atom_to_binary(cmd(Cmd),utf8).

cmdval(Cmd) when is_binary(Cmd) -> cmd(binary_to_existing_atom(Cmd,utf8));
cmdval(Cmd) -> Cmd.

statusstr(Status) when is_integer(Status) ->
    {_, StatusStr, _} = err(Status),
    list_to_binary(StatusStr).

-spec(encode(PDU :: map()) -> {ok, HEX_STRING :: binary()} | {error, binary()}).
encode(PDU) when is_map(PDU) ->
    case pack(json2internal(from_enum(PDU))) of
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
    decode_bin(Bin);
decode(_) ->
    {error, <<"Input to decode should be Hex String">>}.

-spec(decode_bin(binary()) -> {ok, PDU :: map()} | {error, binary()}).
decode_bin(Bin) when is_binary(Bin) ->
    case unpack_map(Bin) of
        {error, _, S, _} ->
            {error, list_to_binary(element(3, err(S)))};
        PDU ->
            {ok, internal2json(to_enum(PDU))}
    end.


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
b2a(<<"type">>) -> type;
b2a(<<"error">>) -> error;
b2a(<<"format">>) -> format;
b2a(<<"number">>) -> number;
b2a(<<"details">>) -> details;
b2a(<<"service">>) -> service;
b2a(<<"dl_name">>) -> dl_name;
b2a(<<"password">>) -> password;
b2a(<<"addr_npi">>) -> addr_npi;
b2a(<<"addr_ton">>) -> addr_ton;
b2a(<<"esme_addr">>) -> esme_addr;
b2a(<<"esm_class">>) -> esm_class;
b2a(<<"dest_flag">>) -> dest_flag;
b2a(<<"sm_length">>) -> sm_length;
b2a(<<"system_id">>) -> system_id;
b2a(<<"dest_port">>) -> dest_port;
b2a(<<"time_unit">>) -> time_unit;
b2a(<<"dpf_result">>) -> dpf_result;
b2a(<<"message_id">>) -> message_id;
b2a(<<"command_id">>) -> command_id;
b2a(<<"error_code">>) -> error_code;
b2a(<<"final_date">>) -> final_date;
b2a(<<"data_coding">>) -> data_coding;
b2a(<<"system_type">>) -> system_type;
b2a(<<"source_addr">>) -> source_addr;
b2a(<<"protocol_id">>) -> protocol_id;
b2a(<<"network_type">>) -> network_type;
b2a(<<"dest_address">>) -> dest_address;
b2a(<<"service_type">>) -> service_type;
b2a(<<"callback_num">>) -> callback_num;
b2a(<<"number_digits">>) -> number_digits;
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
b2a(<<"message_payload">>) -> message_payload;
b2a(<<"sequence_number">>) -> sequence_number;
b2a(<<"source_addr_npi">>) -> source_addr_npi;
b2a(<<"source_addr_ton">>) -> source_addr_ton;
b2a(<<"validity_period">>) -> validity_period;
b2a(<<"sar_msg_ref_num">>) -> sar_msg_ref_num;
b2a(<<"destination_addr">>) -> destination_addr;
b2a(<<"dest_bearer_type">>) -> dest_bearer_type;
b2a(<<"dest_network_type">>) -> dest_network_type;
b2a(<<"sm_default_msg_id">>) -> sm_default_msg_id;
b2a(<<"interface_version">>) -> interface_version;
b2a(<<"broadcast_rep_num">>) -> broadcast_rep_num;
b2a(<<"dest_addr_subunit">>) -> dest_addr_subunit;
b2a(<<"source_bearer_type">>) -> source_bearer_type;
b2a(<<"network_error_code">>) -> network_error_code;
b2a(<<"sar_segment_seqnum">>) -> sar_segment_seqnum;
b2a(<<"sar_total_segments">>) -> sar_total_segments;
b2a(<<"user_response_code">>) -> user_response_code;
b2a(<<"registered_delivery">>) -> registered_delivery;
b2a(<<"digit_mode_indicator">>) -> digit_mode_indicator;
b2a(<<"dest_addr_np_country">>) -> dest_addr_np_country;
b2a(<<"sc_interface_version">>) -> sc_interface_version;
b2a(<<"callback_num_pres_ind">>) -> callback_num_pres_ind;
b2a(<<"more_messages_to_send">>) -> more_messages_to_send;
b2a(<<"broadcast_area_success">>) -> broadcast_area_success;
b2a(<<"broadcast_content_type">>) -> broadcast_content_type;
b2a(<<"ms_msg_wait_facilities">>) -> ms_msg_wait_facilities;
b2a(<<"ms_availability_status">>) -> ms_availability_status;
b2a(<<"user_message_reference">>) -> user_message_reference;
b2a(<<"schedule_delivery_time">>) -> schedule_delivery_time;
b2a(<<"replace_if_present_flag">>) -> replace_if_present_flag;
b2a(<<"delivery_failure_reason">>) -> delivery_failure_reason;
b2a(<<"broadcast_area_identifier">>) -> broadcast_area_identifier;
b2a(<<"alert_on_message_delivery">>) -> alert_on_message_delivery;
b2a(<<"additional_status_info_text">>) -> additional_status_info_text;
b2a(<<"broadcast_frequency_interval">>) -> broadcast_frequency_interval;
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
err(<<"ESME_RINVOPTPARAMVAL">>)     -> ?ESME_RINVOPTPARAMVAL;
err({error, _CmdId, ErrCode, _Seq}) -> err(ErrCode);
err(undefined)                      -> err(?ESME_RUNKNOWNERR).

-define(BASE(_Id), #{command_id => cmdstr(_Id),
                     command_status => statusstr(?ESME_ROK),
                     sequence_number => 0}).
-define(M_SYS_ID(_Id), ?BASE(_Id)#{system_id => <<>>}).
-define(M_DST_ADDR(_Id), ?BASE(_Id)#{destination_addr => <<>>}).
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
          broadcast_sm              => ?BASE(cmd(broadcast_sm))
                                            #{broadcast_area_identifier =>
                                                [#{details => <<>>, format => 0}],
                                              broadcast_content_type =>
                                                #{network_type => 0, service => 0},
                                              broadcast_frequency_interval =>
                                                #{number => 0, time_unit => 0},
                                              broadcast_rep_num => 0},
          submit_multi              => ?BASE(cmd(submit_multi))
                                                    #{dest_address => <<>>},

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
                                                       #{unsuccess_sme => <<>>},
          query_broadcast_sm_resp   => ?BASE(cmd(query_broadcast_sm_resp))
                                                    #{broadcast_area_identifier =>
                                                        [#{details => <<>>, format => 0}],
                                                      broadcast_area_success => [0],
                                                      message_state => <<"SCHEDULED">>}},
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
 {"submit_multi",
  "00 00 00 6D 00 00 00 21 00 00 00 00 00 00 00 01 74 65 73 74 00 01 01 31 32 "
  "2E 35 34 2E 32 36 2E 32 38 00 02 01 00 00 00 01 01 01 31 32 2E 32 34 2E 32 "
  "35 2E 36 33 00 CB 40 03 31 35 30 31 30 35 31 35 31 33 32 35 36 39 39 2B 00 "
  "31 35 30 31 30 35 31 35 31 33 32 35 36 39 39 2B 00 17 00 0A 06 0C 74 65 73 "
  "74 20 6D 65 73 73 61 67 65",
  #{}},
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
 {"replace_sm_issue_28",
  "00 00 00 79 00 00 00 07 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 06 03 31 32 37 2E 30 "
  "2E 30 2E 31 00 39 39 30 30 30 30 30 30 30 30 30 30 30 30 30 2B 00 39 39 30 "
  "34 32 37 32 30 30 30 30 30 30 30 30 52 00 10 7F 00 04 24 00 19 6D 79 5F 6D "
  "65 73 73 61 67 65 5F 70 61 79 6C 6F 61 64 5F 30 30 30 30 35 00",
  #{}},
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
 {"submit_multi_resp_issue_63",
  "00 00 00 A6 80 00 00 21 00 00 00 00 00 00 00 01 74 68 69 73 5F 63 6F 75 6C "
  "64 5F 62 65 5F 61 5F 6D 65 73 73 61 67 65 5F 69 64 00 06 01 01 08 31 36 38 "
  "2E 31 32 33 2E 32 33 34 2E 33 32 31 00 00 00 00 FF 01 04 00 31 32 37 2E 30 "
  "2E 30 2E 31 00 00 00 00 0F 01 02 06 31 36 38 2E 31 32 33 2E 32 33 34 2E 33 "
  "32 31 00 00 00 04 00 01 01 01 31 39 32 2E 31 2E 31 2E 31 30 00 00 00 04 FF "
  "01 00 00 31 36 38 2E 31 32 33 2E 32 33 34 2E 33 32 31 00 00 00 00 0F 01 02 "
  "0A 31 39 32 2E 31 2E 31 2E 31 30 00 00 00 01 12",
  #{}},
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
          destination_addr => <<"168.123.234.321">>}],
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
    validity_period => <<>>}}
]).

packunpack_test_() ->
    {inparallel,
     [{T, fun() ->
            Bin = list_to_binary([binary_to_integer(B, 16) || B <- re:split(L, " ")]),
            SMPP = unpack_map(Bin),
            E1 = to_enum(internal2json(SMPP)),
            if E /= E1 ->
                    ?debugFmt("~n~s~nExpected : ~p~n"
                                "Got      : ~p", [T, E, E1]);
                true -> ok
            end,
            ?assertEqual(E, E1),
            {ok, NewBin} = pack(SMPP),
            NewSMPP = unpack_map(NewBin),
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
                ?assertEqual(Ex, D),
                ?assertEqual(true, is_map(D)),
                ?assertEqual({ok, D}, decode(re:replace(P,"\s","",[global,{return,list}]))),
                ?assertEqual({ok, D}, decode(re:replace(P,"\s","",[global,{return,binary}]))),
                {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                ?assertEqual(true, is_binary(E)),
                ?assertEqual({ok, D}, decode(E))
            end}
        || {T,P,Ex} <- ?TESTS]
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
                #{} = D = unpack_map(Bin),
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
                    case encode(Pdu) of
                        {error, _ , Error, _} ->
                            ?assertEqual(ok, err(Error));
                        {ok, Bin} ->
                            {ok, Pdu2} = decode(Bin),
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
                 {ok, D} = decode(I),
                 ?assertEqual(O, maps:without(?IGNORE_FIELDS, D)),
                 {ok, E} = encode(jsx:decode(jsx:encode(D), [return_maps])),
                 ?assertEqual(true, is_binary(E)),
                 {ok, D1} = decode(E),
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
    #{schema := Schema} = info(),
    ?assertEqual(true, is_binary(jsx:encode(Schema))).

-endif.