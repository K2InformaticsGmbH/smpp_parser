-module(smpp_msg).

-export([encode_ucs2/1, decode_ucs2/1, encode_latin1_ascii/1, decode_latin1_ascii/1]).

encode_ucs2(Msg) when is_binary(Msg) ->
    encode_ucs2(unicode:characters_to_list(Msg, unicode));
encode_ucs2(Msg) when is_list(Msg) ->
    SM1 = unicode:characters_to_list(Msg, unicode),
    SM2 = unicode:characters_to_binary(SM1, unicode, utf16),
    re:replace(SM2, "\"", "\\\\\"", [global, {return, binary}]).

decode_ucs2(Msg) when is_binary(Msg) ->
    decode_ucs2(binary_to_list(Msg));
decode_ucs2(Msg) when is_list(Msg) ->
    io:format("smppp_msg : ~p~nsize : ~p~n", [Msg, length(Msg)]),
    re:replace(ucs2_to_utf16(Msg), "\"", "\\\\\"", [global, {return, binary}]).

decode_latin1_ascii(Msg) when is_binary(Msg) ->
    decode_latin1_ascii(unicode:characters_to_list(Msg, unicode));
decode_latin1_ascii(Msg) ->
    Msg1 = unicode:characters_to_binary(udh_to_escaped_utf8(Msg)),
    re:replace(Msg1, "\"", "\\\\\"", [global, {return, binary}]).

ucs2_to_utf16({cp, CPList}) ->
    case unicode:characters_to_binary(CPList, utf16, utf8) of
        {error, ConvertedBin, [[Failed]|Rest]} ->
            <<ConvertedBin/binary, (Failed bsr 8), (Failed band 16#00FF),
              (ucs2_to_utf16({cp, Rest}))/binary>>;
        ConvertedBin when is_binary(ConvertedBin) -> ConvertedBin
    end;
% ucs2_to_utf16(Msg) when is_list(Msg) ->
%     CPList = ucs2_to_utf16_cp(Msg, []),
%     ucs2_to_utf16({cp, CPList}).
ucs2_to_utf16(Msg) when is_list(Msg) ->
    case cut_udh(Msg) of
        {Udh, SM} ->
            CPList = ucs2_to_utf16_cp(SM, []),
            list_to_binary([udh_to_escaped_utf8(Udh),
                            ucs2_to_utf16({cp, CPList})]);
        Msg ->
            CPList = ucs2_to_utf16_cp(Msg, []),
            ucs2_to_utf16({cp, CPList})
    end.
    % CPList = ucs2_to_utf16_cp(Msg, []),
    % ucs2_to_utf16({cp, CPList}).

cut_udh([5,0,3,R,C,N | Rest])       -> {[5,0,3,R,C,N],      Rest};
cut_udh([6,8,4,RH,RL,C,N | Rest])   -> {[6,8,4,RH,RL,C,N],  Rest};
cut_udh(M)                          -> M.  

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

-define(MAX_MSG_LENGTH_7BIT,        160).
-define(MAX_MSG_PART_LENGTH_7BIT,   152).

encode_latin1_ascii(Msg) when is_binary(Msg) ->
    encode_latin1_ascii(unicode:characters_to_list(Msg, unicode));
encode_latin1_ascii(Msg) when is_list(Msg) ->
    case lists:max(Msg) > $€ of % Unicode code point 8364 (10x20AC)
        true -> {error, ucs2_encoding};
        false ->
            try unicode_to_escaped_latin1(Msg, {[[]], ?MAX_MSG_LENGTH_7BIT}) of
                [MsgLatin1] -> MsgLatin1;
                Messages when is_list(Messages), length(Messages) > 1 ->
                    Messages
            catch
                _:Exception ->
                    io:format("!!! error encoding use ucs2 ~p~n", [Exception]),
                    {error, ucs2_encoding}
            end
    end.

-define(EP(_Cp, _C),
unicode_to_escaped_latin1([_Cp|_R]=_OMsg, {[_Msg|_Segs], _Max}) ->
    _Char = lists:flatten([_C]),
    _MsgLen = length(_Msg),
    if _MsgLen + length(_Char) > _Max ->
        if _Max == ?MAX_MSG_LENGTH_7BIT ->
            __RollBack = _MsgLen - ?MAX_MSG_PART_LENGTH_7BIT,
            {__Next, __Concat} =
                case lists:split(__RollBack, _Msg) of
                    {_N, [16#1B | _Rest]} -> {_N ++ [16#1B], _Rest};
                    {_N, _Cc} -> {_N, _Cc}
                end,
            unicode_to_escaped_latin1(
                _R, {[_Char ++ __Next, to_bin(__Concat) | _Segs],
                ?MAX_MSG_PART_LENGTH_7BIT}
            );
        true ->
            unicode_to_escaped_latin1(
                _R, {[_Char, to_bin(_Msg) | _Segs], ?MAX_MSG_PART_LENGTH_7BIT}
            )
        end;
    true ->
        unicode_to_escaped_latin1(
            _R, {[_Char ++ _Msg | _Segs], _Max}
        )
    end
).

% remove 1B escapes for implicit escape characters
to_bin(Msg) when is_list(Msg) -> to_bin(Msg, []).
to_bin([], Acc) -> list_to_binary(Acc);
to_bin([16#5B, 16#1B | Rest], Acc) -> to_bin(Rest, [16#5B | Acc]);
to_bin([16#5C, 16#1B | Rest], Acc) -> to_bin(Rest, [16#5C | Acc]);
to_bin([16#5D, 16#1B | Rest], Acc) -> to_bin(Rest, [16#5D | Acc]);
to_bin([16#5E, 16#1B | Rest], Acc) -> to_bin(Rest, [16#5E | Acc]);
to_bin([16#7B, 16#1B | Rest], Acc) -> to_bin(Rest, [16#7B | Acc]);
to_bin([16#7C, 16#1B | Rest], Acc) -> to_bin(Rest, [16#7C | Acc]);
to_bin([16#7D, 16#1B | Rest], Acc) -> to_bin(Rest, [16#7D | Acc]);
to_bin([16#7E, 16#1B | Rest], Acc) -> to_bin(Rest, [16#7E | Acc]);
to_bin([C | Rest], Acc) -> to_bin(Rest, [C | Acc]).

% The following Escaped-Latin1 characters are in GSM 03.38 Basic Character Set
%  Extension table : implicit escape byte (1B) insertsion in shot message at
%  GSM level (re)encoding
%------------------------------------------------------------------------------
% Char  Escaped-Latin1  GSM 03.38
% ----  --------------  ---------
% [     5B              1B 3C
% \     5C              1B 2F
% ]     5D              1B 3E
% ^     5E              1B 14
% {     7B              1B 28
% |     7C              1B 40
% }     7D              1B 29
% ~     7E              1B 3D
%-----------------------------------------------------------------------------

unicode_to_escaped_latin1([], {[Msg | Msgs], _}) when not is_binary(Msg) ->
    [to_bin(Msg) | Msgs];
unicode_to_escaped_latin1([], {Messages, _}) -> Messages;

% 0x00 - 0x0F (0 to 15)
?EP(0,   16#00); ?EP($£,  16#01); ?EP($$,  16#02); ?EP($¥,  16#03);
?EP($è,  16#04); ?EP($é,  16#05); ?EP($ù,  16#06); ?EP($ì,  16#07);
?EP($ò,  16#08); ?EP($Ç,  16#09); ?EP($\n, 16#0A); ?EP($Ø,  16#0B);
?EP($\f, 16#0C); ?EP($\r, 16#0D); ?EP($Å,  16#0E); ?EP($å,  16#0F);

% 0x10 - 0x1F (16 to 31)
?EP($Δ,  16#10); ?EP($_,  16#11); ?EP($Φ,  16#12); ?EP($Γ,  16#13);
?EP($Λ,  16#14); ?EP($Ω,  16#15); ?EP($Π,  16#16); ?EP($Ψ,  16#17);
?EP($Σ,  16#18); ?EP($Θ,  16#19); ?EP($Ξ,  16#1A); ?EP($\e, 16#1B);
?EP($Æ,  16#1C); ?EP($æ,  16#1D); ?EP($ß,  16#1E); ?EP($É,  16#1F);

% 0x20 - 0x2F (32 to 47)
?EP($\s, 16#20); ?EP($!,  16#21); ?EP($\", 16#22); ?EP($#,  16#23);
?EP($%,  16#25); ?EP($&,  16#26); ?EP($',  16#27); ?EP($(,  16#28);
?EP($),  16#29); ?EP($*,  16#2A); ?EP($+,  16#2B); ?EP($,,  16#2C);
?EP($-,  16#2D); ?EP($.,  16#2E); ?EP($/,  16#2F);

% 0x30 - 0x3F (48 to 63)
?EP($0,  16#30); ?EP($1,  16#31); ?EP($2,  16#32); ?EP($3,  16#33);
?EP($4,  16#34); ?EP($5,  16#35); ?EP($6,  16#36); ?EP($7,  16#37);
?EP($8,  16#38); ?EP($9,  16#39); ?EP($:,  16#3A); ?EP($;,  16#3B);
?EP($<,  16#3C); ?EP($=,  16#3D); ?EP($>,  16#3E); ?EP($?,  16#3F);

% 0x40 - 0x4F (64 to 79)
?EP($@,  16#40); ?EP($A,  16#41); ?EP($B,  16#42); ?EP($C,  16#43);
?EP($D,  16#44); ?EP($E,  16#45); ?EP($F,  16#46); ?EP($G,  16#47);
?EP($H,  16#48); ?EP($I,  16#49); ?EP($J,  16#4A); ?EP($K,  16#4B);
?EP($L,  16#4C); ?EP($M,  16#4D); ?EP($N,  16#4E); ?EP($O,  16#4F);

% 0x50 - 0x5F (79 to 95)
?EP($P,  16#50); ?EP($Q,  16#51); ?EP($R,  16#52); ?EP($S,  16#53);
?EP($T,  16#54); ?EP($U,  16#55); ?EP($V,  16#56); ?EP($W,  16#57);
?EP($X,  16#58); ?EP($Y,  16#59); ?EP($Z,  16#5A); ?EP($[,  [16#5B, 16#1B]);
?EP($\\, [16#5C, 16#1B]); ?EP($], [16#5D, 16#1B]); ?EP($^, [16#5E, 16#1B]);

% 0x60 - 0x6F (96 to 111)
?EP($¿,  16#60); ?EP($a,  16#61); ?EP($b,  16#62); ?EP($c,  16#63);
?EP($d,  16#64); ?EP($e,  16#65); ?EP($f,  16#66); ?EP($g,  16#67);
?EP($h,  16#68); ?EP($i,  16#69); ?EP($j,  16#6A); ?EP($k,  16#6B);
?EP($l,  16#6C); ?EP($m,  16#6D); ?EP($n,  16#6E); ?EP($o,  16#6F);

% 0x70 - 0x7F (112 to 127)
?EP($p,  16#70); ?EP($q,  16#71); ?EP($r,  16#72); ?EP($s,  16#73);
?EP($t,  16#74); ?EP($u,  16#75); ?EP($v,  16#76); ?EP($w,  16#77);
?EP($x,  16#78); ?EP($y,  16#79); ?EP($z,  16#7A); ?EP(${,  [16#7B, 16#1B]);
?EP($|,  [16#7C, 16#1B]); ?EP($}, [16#7D, 16#1B]); ?EP($~,  [16#7E, 16#1B]);
?EP($à,  16#7F);

% extras
?EP($¡,  16#A1); ?EP($¤,  16#A4); ?EP($§,  16#A7); ?EP($Ä,  16#C4);
?EP($Ñ,  16#D1); ?EP($Ö,  16#D6); ?EP($Ü,  16#DC); ?EP($ä,  16#E4);
?EP($ñ,  16#F1); ?EP($ö,  16#F6); ?EP($ø,  16#F8); ?EP($ü,  16#FC);

% Max
?EP($€, [16#65, 16#1B]);

% all other cases
unicode_to_escaped_latin1([C | _] = M,  _) -> error({not_latin1, C, M}).

%------------------------------------------------------------------------------
% Ref : https://en.wikipedia.org/wiki/Concatenated_SMS#Sending_a_concatenated_SMS_using_a_User_Data_Header
%
% 8-bit CSMS
%  Field 1 (1 octet): Length of User Data Header, in this case 05.
%  Field 2 (1 octet): Information Element Identifier, equal to 00 (Concatenated
%                     short messages, 8-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 03
%  Field 4 (1 octet): 00-FF, CSMS reference number, must be same for all the
%                     SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value
%                     shall start at 1 and increment for every short message
%                     which makes up the concatenated short message. If the
%                     value is zero or greater than the value in Field 5 then
%                     the receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
udh_to_escaped_utf8([5,0,3,R,C,N | Rest]) ->
    ["\\u0005\\u0000\\u0003",
     io_lib:format("\\u~4.16.0B\\u~4.16.0B\\u~4.16.0B", [R,C,N])
     | Rest];

% 16-bit CSMS
%  It is possible to use a 16 bit CSMS reference number in order to reduce the
%  probability that two different concatenated messages are sent with identical
%  reference numbers to a receiver. In this case, the User Data Header shall be:
%
%  Field 1 (1 octet): Length of User Data Header (UDL), in this case 06.
%  Field 2 (1 octet): Information Element Identifier, equal to 08 (Concatenated
%                     short messages, 16-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 04
%  Field 4 (2 octets): 0000-FFFF, CSMS reference number, must be same for all
%                     the SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value shall
%                     start at 1 and increment for every short message which
%                     makes up the concatenated short message. If the value is
%                     zero or greater than the value in Field 5 then the
%                     receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
udh_to_escaped_utf8([6,8,4,RH,RL,C,N | Rest]) ->
    ["\\u0006\\u0008\\u0004",
     io_lib:format("\\u~4.16.0B\\u~2.16.0B~2.16.0B\\u~4.16.0B", [RH,RL,C,N])
     | Rest];

% No UDH headers detected
udh_to_escaped_utf8(M) -> M.
