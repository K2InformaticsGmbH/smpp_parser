-module(smpp).
-include("smpp_globals.hrl").

-export([pack/1, unpack/1, unpack_map/1, unpack/2, json2internal/1,
         internal2json/1, encode/1, decode/1, decode_bin/1, info/0]).

-export([err/1, cmd/1, cmdstr/1, to_enum/1, from_enum/1, encode_msg/1,
         decode_msg/1]).

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
map_to_rec(failed_broadcast_area_identifier, DA) ->
    Rec = rec_type(broadcast_area),
    list_to_tuple([Rec | [maps:get(K, DA) || K <- rec_info(Rec)]]);
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
rec_info(dest_subaddress) ->
    record_info(fields, subaddress);
rec_info(Type) ->
    io:format(user, "~p:~p:~p unknown ~p~n", [?MODULE, ?FUNCTION_NAME, ?LINE, Type]),
    [].

rec_type(dest_subaddress) -> subaddress;
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

encode_msg(#{<<"data_coding">> := EncodingScheme, <<"short_message">> := Msg} = Pdu) ->
    Pdu#{<<"short_message">> => encode_msg(EncodingScheme, Msg)};
encode_msg(#{data_coding := EncodingScheme, short_message := Msg} = Pdu) ->
    Pdu#{short_message => encode_msg(EncodingScheme, Msg)};
encode_msg(Pdu) -> Pdu.

encode_msg(EncodingScheme, Msg) when is_binary(EncodingScheme) ->
    encode_msg(enc(EncodingScheme), Msg);
encode_msg(?ENCODING_SCHEME_LATIN_1, Msg) -> Msg;
encode_msg(?ENCODING_SCHEME_IA5_ASCII, Msg) -> Msg;
encode_msg(?ENCODING_SCHEME_UCS2, Msg) -> ucs2_encoding(Msg);
encode_msg(EncodingScheme, Msg) when is_binary(Msg) ->
    encode_msg(EncodingScheme, binary_to_list(Msg));
encode_msg(_, Msg) when is_list(Msg) ->
    case io_lib:printable_list(Msg) of
        true -> list_to_binary(Msg);
        false -> base64:decode(Msg)
    end.

decode_msg(#{<<"data_coding">> := EncodingScheme, <<"short_message">> := Msg} = Pdu) ->
    Pdu#{<<"short_message">> => decode_msg(EncodingScheme, Msg)};
decode_msg(#{data_coding := EncodingScheme, short_message := Msg} = Pdu) ->
    Pdu#{short_message => decode_msg(EncodingScheme, Msg)};
decode_msg(Pdu) -> Pdu.

decode_msg(EncodingScheme, Msg) when is_binary(EncodingScheme) ->
    decode_msg(enc(EncodingScheme), Msg);
decode_msg(?ENCODING_SCHEME_UCS2, Msg) -> decode_ucs2(Msg);
decode_msg(?ENCODING_SCHEME_LATIN_1, Msg) -> Msg;
decode_msg(?ENCODING_SCHEME_IA5_ASCII, Msg) -> Msg;
decode_msg(EncodingScheme, Msg) when is_binary(Msg) ->
    decode_msg(EncodingScheme, binary_to_list(Msg));
decode_msg(_, Msg) when is_list(Msg) ->
    case io_lib:printable_list(Msg) of
        true -> list_to_binary(Msg);
        false -> base64:encode(Msg)
    end.

ucs2_encoding(Msg) when is_binary(Msg) ->
    ucs2_encoding(unicode:characters_to_list(Msg, unicode));
ucs2_encoding(Msg) when is_list(Msg) ->
    SM1 = unicode:characters_to_list(Msg, unicode),
    SM2 = unicode:characters_to_binary(SM1, unicode, utf16),
    re:replace(SM2, "\"", "\\\\\"", [global, {return, binary}]).

decode_ucs2(Msg) when is_binary(Msg) ->
    decode_ucs2(binary_to_list(Msg));
decode_ucs2(Msg) when is_list(Msg) ->
    re:replace(ucs2_to_utf16(Msg), "\"", "\\\\\"", [global, {return, binary}]).

ucs2_to_utf16({cp, CPList}) ->
    case unicode:characters_to_binary(CPList, utf16, utf8) of
        {error, ConvertedBin, [[Failed]|Rest]} ->
            <<ConvertedBin/binary, (Failed bsr 8), (Failed band 16#00FF),
              (ucs2_to_utf16({cp, Rest}))/binary>>;
        ConvertedBin when is_binary(ConvertedBin) -> ConvertedBin
    end;
ucs2_to_utf16(Msg) when is_list(Msg) ->
    CPList = ucs2_to_utf16_cp(Msg, []),
    ucs2_to_utf16({cp, CPList}).

ucs2_to_utf16_cp([], Result) -> lists:reverse(Result);
% Erlang DOC: An integer in the range 16#D800 to 16#DFFF (invalid range
%  reserved for UTF-16 surrogate pairs)
ucs2_to_utf16_cp([A,B|Rest], Result) when A >= 16#D8, A =< 16#DF ->
    ucs2_to_utf16_cp(
      Rest, lists:reverse(
                lists:flatten(
                    [io_lib:format("\\u~2.16.0B~2.16.0B", [A,B])]
                )) ++ Result);
ucs2_to_utf16_cp([A,B|Rest], Result) ->
    ucs2_to_utf16_cp(Rest, [(A bsl 8) + B | Result]).

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
b2a(<<"reserved">>) -> reserved;
b2a(<<"esme_addr">>) -> esme_addr;
b2a(<<"esm_class">>) -> esm_class;
b2a(<<"dest_flag">>) -> dest_flag;
b2a(<<"sm_length">>) -> sm_length;
b2a(<<"system_id">>) -> system_id;
b2a(<<"dest_port">>) -> dest_port;
b2a(<<"time_unit">>) -> time_unit;
b2a(<<"sms_signal">>) -> sms_signal;
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
b2a(<<"its_reply_type">>) -> its_reply_type;
b2a(<<"command_length">>) -> command_length;
b2a(<<"message_payload">>) -> message_payload;
b2a(<<"dest_subaddress">>) -> dest_subaddress;
b2a(<<"sequence_number">>) -> sequence_number;
b2a(<<"source_addr_npi">>) -> source_addr_npi;
b2a(<<"source_addr_ton">>) -> source_addr_ton;
b2a(<<"validity_period">>) -> validity_period;
b2a(<<"sar_msg_ref_num">>) -> sar_msg_ref_num;
b2a(<<"dest_network_id">>) -> dest_network_id;
b2a(<<"destination_addr">>) -> destination_addr;
b2a(<<"dest_bearer_type">>) -> dest_bearer_type;
b2a(<<"dest_network_type">>) -> dest_network_type;
b2a(<<"sm_default_msg_id">>) -> sm_default_msg_id;
b2a(<<"interface_version">>) -> interface_version;
b2a(<<"broadcast_rep_num">>) -> broadcast_rep_num;
b2a(<<"dest_addr_subunit">>) -> dest_addr_subunit;
b2a(<<"callback_num_atag">>) -> callback_num_atag;
b2a(<<"display_characters">>) -> display_characters;
b2a(<<"number_of_messages">>) -> number_of_messages;
b2a(<<"dest_telematics_id">>) -> dest_telematics_id;
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
b2a(<<"broadcast_content_type_info">>) -> broadcast_content_type_info;
b2a(<<"additional_status_info_text">>) -> additional_status_info_text;
b2a(<<"broadcast_frequency_interval">>) -> broadcast_frequency_interval;
b2a(<<"failed_broadcast_area_identifier">>) -> failed_broadcast_area_identifier;
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
