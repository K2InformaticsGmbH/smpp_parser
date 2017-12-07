-module(utils).

-export([ia5_test/2, ia5_test/5]).

%utils:ia5_test(ShortIdInt2, PasswordString, HanydString, IpString, PortInt).

ia5_test(SystemId, Password, To, Ip, Port) when is_integer(SystemId) ->
    ia5_test(integer_to_list(SystemId), Password, To, Ip, Port);
ia5_test(SystemId, Password, To, Ip, Port) when is_list(SystemId) ->
    {ok, SmscIp} = inet:getaddr(Ip, inet),
    SystemIdCode = hex(SystemId),
    PasswordCode = hex(Password),
    PduLen = 23 + length(SystemIdCode) + length(PasswordCode),
    Len = string:join([hex2(X) || <<X>> <= <<PduLen:32>>], " "),
    Bind = hex2bin(% bind_transmitter
            [Len, " 00 00 00 02 00 00 00 00 00 00 00 01 ",
             string:join(SystemIdCode, " "), " 00 ",
             string:join(PasswordCode, " "), " 00 ",
             "00 34 00 00 00"]),
    ia5_test(spawn, {Bind, SystemIdCode, hex(To), SmscIp, Port}).

ia5_test(spawn, {Bind, From, To, SmscIp, SmscPort}) ->
    spawn(?MODULE, ia5_test, [connect, {Bind, From, To, SmscIp, SmscPort}]);
ia5_test(connect, {Bind, From, To, SmscIp, SmscPort}) ->
    io:format("CONNECT..."),
    case gen_tcp:connect(SmscIp, SmscPort, [{active, false}, binary], 10000) of % 10 seconds
        {error, Error} -> io:format("failed ~p~n", [Error]);
        {ok, Socket} ->
            io:format("connected~n"),
            ia5_test(bind, {Socket, Bind, From, To})
    end;
ia5_test(bind, {Socket, Bind, From, To}) ->
    io:format("sending BIND..."),
    case gen_tcp:send(Socket, Bind) of
        {error, Error} -> io:format("failed ~p~n", [Error]);
        ok ->
            io:format("sent~n"),
            ia5_test(bind_rx, {Socket, From, To})
    end;
ia5_test(bind_rx, {Socket, From, To}) ->
    io:format("receiving BIND resp..."),
    case gen_tcp:recv(Socket, 0, 60000) of
        {error, Error} ->
            io:format("failed ~p~n", [Error]),
            ia5_test(close, Socket);
        {ok, BindResp} ->
            io:format("received : ~s~n", [bin2hex(BindResp)]),
            ia5_test(prepare_msg, {Socket, From, To, 16#00, 16#FF})
    end;
ia5_test(prepare_msg, {Socket, From, To, Start, End}) ->
    io:format("preparing SUBMITs..."),
    case ia5(Start, End, 160) of
        {done, Msg} ->
            io:format("prepared ~p submit~n", [length(Msg)]),
            ia5_test(send_submit, {Socket, From, To, Msg}),
            ia5_test(close, Socket);
        {NewStart, Msg} ->
            io:format("prepared ~p submit~n", [length(Msg)]),
            ia5_test(send_submit, {Socket, From, To, Msg}),
            io:format("wait a second~n"),
            timer:sleep(1000),
            ia5_test(prepare_msg, {Socket, From, To, NewStart, End})
    end;
ia5_test(send_submit, {Socket, From, To, Msg}) ->
    CmdLen = 33 + length(From) + length(To) + length(Msg),
    Submit = hex2bin(% submit_sm
                [len(CmdLen), " 00 00 00 04 00 00 00 00 00 00 00 02 00 05 00 ",
                string:join(From, " "), " 00 00 00 ",
                string:join(To, " "), " 00 00 00 00 00 00 11 00 03 00 ",
                hex2(length(Msg)), " ", string:join(Msg, " ")]),
    io:format("sending submit(~p)...", [length(Msg)]),
    case gen_tcp:send(Socket, Submit) of
        ok ->
            io:format("sent~n"),
            ia5_test(submit_rx, Socket);
        {error, Error} ->
            io:format("failed ~p~n", [Error]),
            ia5_test(close, Socket)
    end;
ia5_test(submit_rx, Socket) ->
    io:format("receiving SUBMIT resp..."),
    case gen_tcp:recv(Socket, 0, 60000) of
        {error, Error} ->
            io:format("failed ~p~n", [Error]),
            ia5_test(close, Socket);
        {ok, SubmitResp} ->
            io:format("received : ~s~n", [bin2hex(SubmitResp)])
    end;
ia5_test(close, Socket) ->
    io:format("DISCONNECT.."),
    gen_tcp:close(Socket),
    io:format("disconnected~n").

ia5(Start, End, Max) -> ia5(Start, End, Max, []).
ia5(Start, End, Max, Buf) when Start =< End ->
    case ia5(Start, Buf) of
        NewBuf when length(NewBuf) < Max ->
            ia5(Start + 1, End, Max, NewBuf);
        _ -> {Start, Buf}
    end;
ia5(_, _, _, Buf) -> {done, Buf}.

ia5(I, Buf) ->
    % 0 - 9 => 16#30 - 16#39
    % ABCDEF => 16#41 - 16#46
    % CR => 16#0D
    % SP => 16#20
    % = => 16#3D
    lists:append([
        Buf,
        [hex2(H) || H <- hex2(I)], ["20"],
        [hex2(H) || H <- string:right(integer_to_list(I),3,$0)], ["20"],
        [hex2(I)], ["0D"]
    ]).

len(Len) ->
    string:join([hex2(X) || <<X>> <= <<Len:32>>], " ").

hex2(Int) ->
    string:right(integer_to_list(Int,16),2,$0).

hex(String) ->
    [hex2(Int) || Int <- String].

hex2bin(Hex) ->
    << <<(binary_to_integer(B, 16))>>
    || B <- re:split(lists:flatten(Hex), " ")>>.

bin2hex(Bin) ->
    string:join([hex2(B) || <<B>> <= Bin], " ").
