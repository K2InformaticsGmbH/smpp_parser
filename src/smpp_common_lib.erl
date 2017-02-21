-module(smpp_common_lib).

%% APIs collected from https://github.com/iamaleksey/common_lib
%% Right of original author applies

-export([is_hex/1,is_dec/1,is_atime/1,is_rtime/1]).
-export([take_until/3]).
-export([ukeymerge/3]).

is_hex([]) ->
    true;
is_hex([Digit|Rest]) when (Digit >= 47) and (Digit =< 57);
                          (Digit >= 65) and (Digit =< 70);
                          (Digit >= 97) and (Digit =< 102) ->
    is_hex(Rest);
is_hex(_String) ->
    false.

is_dec([]) ->
    true;
is_dec([Digit|Rest]) when (Digit >= 48) and (Digit =< 57) ->
    is_dec(Rest);
is_dec(_String) ->
    false.

is_atime([]) ->
    true;
is_atime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2,P]) when P == $+;
                                                                   P == $- ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2]) of
        true ->
            Date   = {list_to_integer([Y1,Y2]),
                      list_to_integer([M1,M2]),
                      list_to_integer([D1,D2])},
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            To_UTC = list_to_integer([N1,N2]),
            case calendar:valid_date(Date) of
                true when Hour < 24, Minute < 60, Second < 60, To_UTC < 49 ->
                    true;
                _Otherwise ->
                    false
            end;
        false ->
            false
    end;
is_atime(_String) ->
    false.


is_rtime([]) ->
    true;
is_rtime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,$0,$0,$0,$R]) ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2]) of
        true ->
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            if
                (Hour < 24) and (Minute < 60) and (Second < 60) ->
                    true;
                true ->
                    false
            end;
        false ->
            false
    end;
is_rtime(_String) ->
    false.

take_until(Binary, Pattern, Size) ->
    case take_until(Binary, Pattern, Size, []) of
        not_found ->
            MinSize = lists:min([Size, size(Binary)]),
            <<UntilSize:MinSize/binary-unit:8, _Rest/binary>> = Binary,
            {error, {not_found, Pattern, UntilSize}};
        {ok, UntilPattern, Rest} ->
            {ok, UntilPattern, Rest}
    end.

take_until(<<>>, _Pattern, _Size, _Acc) ->
    not_found;
take_until(_Binary, Pattern, Size, _Acc) when size(Pattern) > Size ->
    not_found;
take_until(Binary, Pattern, Size, Acc) ->
    Len = size(Pattern),
    case Binary of
        <<Pattern:Len/binary-unit:8, _Rest/binary>> ->
            {ok, list_to_binary(lists:reverse(Acc)), Binary};
        <<Octet:8, Rest/binary>> ->
            take_until(Rest, Pattern, Size - 1, [Octet|Acc])
    end.

ukeymerge(N, List1, List2) ->
    ukeymerge(N, List1, List2, []).

ukeymerge(_N, [], List2, MergedList) ->
    lists:reverse(MergedList) ++ List2;
ukeymerge(_N, List1, [], MergedList) ->
    lists:reverse(MergedList) ++ List1;
ukeymerge(N, [H1|T1], [H2|T2], MergedList)
  when element(N, H1) == element(N, H2) ->
    ukeymerge(N, T1, T2, [H1|MergedList]);
ukeymerge(N, [H1|T1], [H2|T2], MergedList)
  when element(N, H1) < element(N, H2) ->
    ukeymerge(N, T1, [H2|T2], [H1|MergedList]);
ukeymerge(N, [H1|T1], [H2|T2], MergedList) ->
    ukeymerge(N, [H1|T1], T2, [H2|MergedList]).
