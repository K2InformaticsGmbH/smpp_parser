-module(smpp_msg).

-include("smpp_globals.hrl").

-export([encode_msg/2, decode_msg/2]).

encode_msg(_, <<>>) -> <<>>;
encode_msg(_, "") -> "";
encode_msg(EncodingScheme, Msg) when is_binary(EncodingScheme) ->
    encode_msg(smpp:enc(EncodingScheme), Msg);
encode_msg(?ENCODING_SCHEME_LATIN_1, Msg) -> encode_latin1_ascii(Msg);
encode_msg(?ENCODING_SCHEME_IA5_ASCII, Msg) -> encode_latin1_ascii(Msg);
encode_msg(?ENCODING_SCHEME_MC_SPECIFIC, Msg) -> encode_latin1_ascii(Msg);
encode_msg(?ENCODING_SCHEME_UCS2, Msg) -> encode_ucs2(Msg);
encode_msg(EncodingScheme, Msg) when is_binary(Msg) ->
    encode_msg(EncodingScheme, binary_to_list(Msg));
encode_msg(_, Msg) when is_list(Msg) -> base64:decode(Msg).

decode_msg(_, <<>>) -> <<>>;
decode_msg(_, "") -> "";
decode_msg(EncodingScheme, Msg) when is_binary(EncodingScheme) ->
    decode_msg(smpp:enc(EncodingScheme), Msg);
decode_msg(?ENCODING_SCHEME_UCS2, Msg) -> decode_ucs2(Msg);
decode_msg(?ENCODING_SCHEME_LATIN_1, Msg) -> decode_latin1_ascii(Msg);
decode_msg(?ENCODING_SCHEME_IA5_ASCII, Msg) -> decode_latin1_ascii(Msg);
decode_msg(?ENCODING_SCHEME_MC_SPECIFIC, Msg) -> decode_latin1_ascii(Msg);
decode_msg(EncodingScheme, Msg) when is_binary(Msg) ->
    decode_msg(EncodingScheme, binary_to_list(Msg));
decode_msg(_, Msg) when is_list(Msg) -> base64:encode(Msg).

encode_ucs2(Msg) when is_binary(Msg) ->
    encode_ucs2(unicode:characters_to_list(Msg, unicode));
encode_ucs2(Msg) when is_list(Msg) ->
    SM1 = unicode:characters_to_list(Msg, unicode),
    SM2 = unicode:characters_to_binary(SM1, unicode, utf16),
    re:replace(SM2, "\"", "\\\\\"", [global, {return, binary}]).

decode_ucs2(Msg) when is_binary(Msg) ->
    decode_ucs2(binary_to_list(Msg));
decode_ucs2(Msg) when is_list(Msg) ->
    re:replace(ucs2_to_utf16(Msg), "\"", "\\\\\"", [global, {return, binary}]).

decode_latin1_ascii(Msg) when is_binary(Msg) ->
    decode_latin1_ascii(unicode:characters_to_list(Msg, unicode));
decode_latin1_ascii({error, _List, Bin}) ->
    {error, {not_unicode, Bin}};
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
                _:_Exception ->
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

%% ===================================================================
%% TESTS
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
    {inparallel,
        [{Title, ?_assertEqual(Result, encode_msg(Encoding, Msg))}
         || {Title, Encoding, Msg, Result} <-
            [{"empty", <<"Latin 1 (ISO-8859-1)">>, <<>>, <<>>},
             {"base64", <<"KS C 5601">>, base64:encode(<<"Test">>), <<"Test">>},
             {"ia5_max", <<"IA5 (CCITT T.50)/ASCII (ANSI X3.4)">>, <<"äöüéèà€"/utf8>>,
              <<16#E4,16#F6,16#FC,16#05,16#04,16#7F,16#1B,16#65>>},
             {"ucs2_bigger_eur", <<"UCS2 (ISO/IEC-10646)">>, <<"Abc₭"/utf8>>,
              unicode:characters_to_binary(
                            <<"Abc₭"/utf8>>, utf8, utf16
                )}
            ]
        ]
    }.

decode_test_() ->
    {inparallel,
        [{Title, ?_assertEqual(Result, decode_msg(Encoding, Msg))}
         || {Title, Encoding, Msg, Result} <-
            [{"empty", <<"Latin 1 (ISO-8859-1)">>, <<>>, <<>>},
             {"base64", <<"KS C 5601">>, <<"Test">>, base64:encode(<<"Test">>)},
            %  {"ia5_max", <<"IA5 (CCITT T.50)/ASCII (ANSI X3.4)">>,
            %   <<16#E4,16#F6,16#FC,16#05,16#04,16#7F,16#1B,16#65>>,
            %   <<"äöüéèà€"/utf8>>},
             {"ucs2_bigger_eur", <<"UCS2 (ISO/IEC-10646)">>,
              unicode:characters_to_binary(<<"Abc₭"/utf8>>, utf8, utf16),
              <<"Abc₭"/utf8>>}
            ]
        ]
    }.

escape_latin1_length_test_() ->
    % 64 + 64 + 27 + 1 (for €)
    EscapeLatinOne136 =
% 00   01   02   03   04   05   06   07   08   09   0A   0B   0C   0D   0E   0F
       "£"  "$"  "¥"  "è"  "é"  "ù"  "ì"  "ò"  "Ç"  "\n" "Ø"       "\r" "Å"  "å"
% 10   11   12   13   14   15   16   17   18   19   1A   1B   1C   1D   1E   1F
  "Δ"  "_"  "Φ"  "Γ"  "Λ"  "Ω"  "Π"  "Ψ"  "Σ"  "Θ"  "Ξ"       "Æ"  "æ"  "ß"  "É"
% 20   21   22   23   24   25   26   27   28   29   2A   2B   2C   2D   2E   2F
  " "  "!"  "\"" "#"       "%"  "&"  "'"  "("  ")"  "*"  "+"  ","  "-"  "."  "/"
% 30   31   32   33   34   35   36   37   38   39   3A   3B   3C   3D   3E   3F
  "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  ":"  ";"  "<"  "="  ">"  "?"
% 40   41   42   43   44   45   46   47   48   49   4A   4B   4C   4D   4E   4F
  "@"  "A"  "B"  "C"  "D"  "E"  "F"  "G"  "H"  "I"  "J"  "K"  "L"  "M"  "N"  "O"
% 50   51   52   53   54   55   56   57   58   59   5A   5B   5C   5D   5E   5F
  "P"  "Q"  "R"  "S"  "T"  "U"  "V"  "W"  "X"  "Y"  "Z"  "["  "\\" "]"  "^"
% 60   61   62   63   64   65   66   67   68   69   6A   6B   6C   6D   6E   6F
  "¿"  "a"  "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"  "k"  "l"  "m"  "n"  "o"
% 70   71   72   73   74   75   76   77   78   79   7A   7B   7C   7D   7E   7F
  "p"  "q"  "r"  "s"  "t"  "u"  "v"  "w"  "x"  "y"  "z"  "{"  "|"  "}"  "\~" "à"
% 80   81   82   83   84   85   86   87   88   89   8A   8B   8C   8D   8E   8F
% 90   91   92   93   94   95   96   97   98   99   9A   9B   9C   9D   9E   9F
% A0   A1   A2   A3   A4   A5   A6   A7   A8   A9   AA   AB   AC   AD   AE   AF
       "¡"            "¤"            "§"
% B0   B1   B2   B3   B4   B5   B6   B7   B8   B9   BA   BB   BC   BD   BE   BF
% C0   C1   C2   C3   C4   C5   C6   C7   C8   C9   CA   CB   CC   CD   CE   CF
                      "Ä"
% D0   D1   D2   D3   D4   D5   D6   D7   D8   D9   DA   DB   DC   DD   DE   DF
       "Ñ"                      "Ö"                           "Ü"
% E0   E1   E2   E3   E4   E5   E6   E7   E8   E9   EA   EB   EC   ED   EE   EF
                      "ä"
% F0   F1   F2   F3   F4   F5   F6   F7   F8   F9   FA   FB   FC   FD   FE   FF
       "ñ"                      "ö"       "ø"                 "ü"
% 1B   65
       "€",
    EscapeLatinOne145Codes = [
% £      $      ¥      è      é      ù      ì      ò      Ç      \n     Ø
  16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08, 16#09, 16#0A, 16#0B,
% \r     Å      å      Δ      _      Φ      Γ      Λ      Ω      Π      Ψ
  16#0D, 16#0E, 16#0F, 16#10, 16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17,
% Σ      Θ      Ξ      Æ      æ      ß      É      \s     !      "      #
  16#18, 16#19, 16#1A, 16#1C, 16#1D, 16#1E, 16#1F, 16#20, 16#21, 16#22, 16#23,
% %      &      '      (      )      *      +      ,      -      .
  16#25, 16#26, 16#27, 16#28, 16#29, 16#2A, 16#2B, 16#2C, 16#2D, 16#2E,
% /      0      1      2      3      4      5      6      7      8      9
  16#2F, 16#30, 16#31, 16#32, 16#33, 16#34, 16#35, 16#36, 16#37, 16#38, 16#39,
% :      ;      <      =      >      ?      @      A      B      C      D
  16#3A, 16#3B, 16#3C, 16#3D, 16#3E, 16#3F, 16#40, 16#41, 16#42, 16#43, 16#44,
% E      F      G      H      I      J      K      L      M      N      O
  16#45, 16#46, 16#47, 16#48, 16#49, 16#4A, 16#4B, 16#4C, 16#4D, 16#4E, 16#4F,
% P      Q      R      S      T      U      V      W      X      Y      Z
  16#50, 16#51, 16#52, 16#53, 16#54, 16#55, 16#56, 16#57, 16#58, 16#59, 16#5A,
% [      \      ]      ^      ¿      a      b      c      d      e
  16#5B, 16#5C, 16#5D, 16#5E, 16#60, 16#61, 16#62, 16#63, 16#64, 16#65,
% f      g      h      i      j      k      l      m      n      o      p
  16#66, 16#67, 16#68, 16#69, 16#6A, 16#6B, 16#6C, 16#6D, 16#6E, 16#6F, 16#70,
% q      r      s      t      u      v      w      x      y      z      {
  16#71, 16#72, 16#73, 16#74, 16#75, 16#76, 16#77, 16#78, 16#79, 16#7A, 16#7B,
% |      }      ~      à      ¡      ¤      §      Ä      Ñ
  16#7C, 16#7D, 16#7E, 16#7F, 16#A1, 16#A4, 16#A7, 16#C4, 16#D1,
% Ö      Ü      ä      ñ      ö      ø      ü             €
  16#D6, 16#DC, 16#E4, 16#F1, 16#F6, 16#F8, 16#FC, 16#1B, 16#65
    ],

    {inparallel,
        [{Title,
            ?_assertEqual(
                Target,
                unicode_to_escaped_latin1(
                    SubmitSm,
                    {[[]], ?MAX_MSG_LENGTH_7BIT}
                )
            )}
         || {Title, SubmitSm, Target} <-
            [{"all chars in single segment", EscapeLatinOne136,
                [list_to_binary(EscapeLatinOne145Codes)]},
             {"two segments", EscapeLatinOne136
                                ++ lists:duplicate(7,$a)
                                ++ lists:duplicate(7,$b)
                                ++ "€",
                [list_to_binary(lists:duplicate(7,$b) ++ [16#1B,16#65]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(7,$a))]},
             {"two segments doesn't split at double € bytes", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "€"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#1B, 16#65 | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments two double €€ bytes", EscapeLatinOne136
                                ++ lists:duplicate(4,$a)
                                ++ "€€"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#1B, 16#65 | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(4,$a)
                                    ++ [16#1B, 16#65])]},

             {"two segments doesn't split at [", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "["
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#5B | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at \\", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "\\"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#5C | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at ]", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "]"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#5D | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at ^", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "^"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#5E | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at {", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "{"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#7B | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at |", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "|"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#7C | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at }", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "}"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#7D | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]},
             {"two segments doesn't split at \~", EscapeLatinOne136
                                ++ lists:duplicate(6,$a)
                                ++ "\~"
                                ++ lists:duplicate(8,$b),
                [list_to_binary([16#7E | lists:duplicate(8,$b)]),
                 list_to_binary(EscapeLatinOne145Codes
                                    ++ lists:duplicate(6,$a))]}
            ]
        ]
    }.

-endif. % TEST