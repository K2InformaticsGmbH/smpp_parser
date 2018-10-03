-module(smpp_3_4).

-include_lib("eunit/include/eunit.hrl").
-include("../src/smpp_globals.hrl").

-define(TESTS, [
	{"submit_sm_resp(ESME_RINVMSGLEN)",
		"00000010800000040000000100000001",
		{?COMMAND_ID_SUBMIT_SM_RESP, ?ESME_RINVMSGLEN, 1, []}},
	{"submit_sm_resp(ESME_RTHROTTLED)",
		"00000010800000040000005800000001",
		{?COMMAND_ID_SUBMIT_SM_RESP, ?ESME_RTHROTTLED, 1, []}},
	{"bind_transceiver_resp(ESME_RALYBND)",
		"00000010800000090000000500000001",
		{?COMMAND_ID_BIND_TRANSCEIVER_RESP, ?ESME_RALYBND, 1, []}},
	{"bind_transceiver_resp(ESME_RBINDFAIL)",
		"00000010800000090000000D00000001",
		{?COMMAND_ID_BIND_TRANSCEIVER_RESP, ?ESME_RBINDFAIL, 1, []}},
	{"submit_sm_resp(ESME_RMSGQFUL)",
		"00000010800000040000001400000001",
		{?COMMAND_ID_SUBMIT_SM_RESP, ?ESME_RMSGQFUL, 1, []}}
]).

packunpack_test_() ->
    {inparallel,
     [{T, fun() -> packunpack(Pdu, Parsed) end}
      || {T, Pdu, Parsed} <- ?TESTS]
    }.

hex2bin([]) -> <<>>;
hex2bin([A,B|Rest]) ->
	<<(list_to_integer([A,B], 16)), (hex2bin(Rest))/binary>>.

packunpack(Pdu, Parsed) ->
	Bin = hex2bin(Pdu),
	?assertEqual({ok, Parsed}, smpp_operation:unpack(Bin)),
	Packed = smpp_operation:pack(Parsed),
	?assertEqual({ok, Bin}, Packed).
