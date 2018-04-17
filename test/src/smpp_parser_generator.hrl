%% -----------------------------------------------------------------------------
%%
%% smpp_parser_generator.hrl: SMPP - test data generator.
%%
%% Copyright (c) 2012-18 K2 Informatics GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-ifndef(SMPP_PARSER_GENERATOR_HRL).
-define(SMPP_PARSER_GENERATOR_HRL, true).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("smpp_pdu.hrl").

-define(ALIVE_COUNTER, 500).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?D("~n time (ms)          ===  ~12.. B rule: ~s ~n",
        [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?D("~n memory (bytes)     ===  ~12.. B rule: ~s ~n",
        [_MemorySize, atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).

-ifdef(NODEBUG).
-define(D(Format), undefined).
-define(D(Format, Args), undefined).
-else.
-define(D(Format), ?D(Format, [])).
-define(D(Format, Args),
    io:format(user, "~p:~p:~p ===> "Format,
[?MODULE, ?FUNCTION_NAME, ?LINE | Args])).
-endif.

-define(D_CT(Format, Args),
    ct:pal(info, ?MAX_IMPORTANCE, "~p:~p:~p ===> "Format,
[?MODULE, ?FUNCTION_NAME, ?LINE | Args])).

-define(ESME, [
    "00000000",
    "00000001",
    "00000002",
    "00000003",
    "00000004",
    "00000005",
    "00000006",
    "00000007",
    "00000008",
    "0000000A",
    "0000000B",
    "0000000C",
    "0000000D",
    "0000000E",
    "0000000F",
    "00000011",
    "00000013",
    "00000014",
    "00000015",
    "00000033",
    "00000034",
    "00000040",
    "00000042",
    "00000043",
    "00000044",
    "00000045",
    "00000048",
    "00000049",
    "00000050",
    "00000051",
    "00000053",
    "00000054",
    "00000055",
    "00000058",
    "00000061",
    "00000062",
    "00000063",
    "00000064",
    "00000065",
    "00000066",
    "00000067",
    "000000C0",
    "000000C1",
    "000000C2",
    "000000C3",
    "000000C4",
    "000000FE",
    "000000FF",
    "00000100",
    "00000101",
    "00000102",
    "00000103",
    "00000104",
    "00000105",
    "00000106",
    "00000107",
    "00000108",
    "00000109",
    "0000010A",
    "0000010B",
    "0000010C",
    "0000010D",
    "0000010E",
    "0000010F",
    "00000110",
    "00000111",
    "00000112"
]).
-define(ESME_LENGTH, length(?ESME)).

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

-define(FUNCTIONS, [
    encode_decode,
    pack_unpack,
    pack_unpack_map
]).
-define(FUNCTIONS_LAST, lists:last(?FUNCTIONS)).

% TRUE: compacted / FALSE: detailed.
-define(GENERATE_COMPACTED, list_to_atom(string:to_lower(
    os:getenv("GENERATE_COMPACTED", "true")))).

-define(LOGGING, list_to_atom(string:to_lower(os:getenv("LOGGING", "false")))).

-define(MAX_BASIC, list_to_integer(os:getenv("MAX_BASIC", "50"))).
-define(MAX_DEST, 12).
-define(MAX_OPERATION, ?MAX_BASIC * 5).
-define(MAX_RESPONSE_TLV, 3).
-define(MAX_REQUEST_TLV, 6).
-define(MAX_TLV, 10).
-define(MAX_UNSUCCESS, 6).

-define(MCC, [
    "202",
    "204",
    "206",
    "208",
    "212",
    "213",
    "214",
    "216",
    "218",
    "219",
    "220",
    "222",
    "226",
    "228",
    "230",
    "231",
    "232",
    "234",
    "235",
    "238",
    "240",
    "242",
    "244",
    "246",
    "247",
    "248",
    "250",
    "255",
    "257",
    "259",
    "260",
    "262",
    "266",
    "268",
    "270",
    "272",
    "274",
    "276",
    "278",
    "280",
    "282",
    "283",
    "284",
    "286",
    "288",
    "289",
    "290",
    "292",
    "293",
    "294",
    "295",
    "297",
    "302",
    "308",
    "310",
    "311",
    "312",
    "316",
    "330",
    "334",
    "338",
    "340",
    "342",
    "344",
    "346",
    "348",
    "350",
    "352",
    "354",
    "356",
    "358",
    "360",
    "362",
    "363",
    "364",
    "365",
    "366",
    "368",
    "370",
    "372",
    "374",
    "376",
    "400",
    "401",
    "402",
    "404",
    "405",
    "410",
    "412",
    "413",
    "414",
    "415",
    "416",
    "417",
    "418",
    "419",
    "420",
    "421",
    "422",
    "424",
    "425",
    "426",
    "427",
    "428",
    "429",
    "430",
    "431",
    "432",
    "434",
    "436",
    "437",
    "438",
    "440",
    "441",
    "450",
    "452",
    "454",
    "455",
    "456",
    "457",
    "460",
    "466",
    "467",
    "470",
    "472",
    "502",
    "505",
    "510",
    "514",
    "515",
    "520",
    "525",
    "528",
    "530",
    "537",
    "539",
    "540",
    "541",
    "542",
    "544",
    "545",
    "546",
    "547",
    "548",
    "549",
    "550",
    "552",
    "553",
    "555",
    "602",
    "603",
    "604",
    "605",
    "606",
    "607",
    "608",
    "609",
    "610",
    "611",
    "612",
    "613",
    "614",
    "615",
    "616",
    "617",
    "618",
    "619",
    "620",
    "621",
    "622",
    "623",
    "624",
    "625",
    "626",
    "627",
    "628",
    "629",
    "630",
    "631",
    "632",
    "633",
    "634",
    "635",
    "636",
    "637",
    "638",
    "639",
    "640",
    "641",
    "642",
    "643",
    "645",
    "646",
    "647",
    "648",
    "649",
    "650",
    "651",
    "652",
    "653",
    "654",
    "655",
    "657",
    "659",
    "702",
    "704",
    "706",
    "708",
    "710",
    "712",
    "714",
    "716",
    "722",
    "724",
    "730",
    "732",
    "734",
    "736",
    "738",
    "740",
    "744",
    "746",
    "748",
    "750",
    "901"
]).
-define(MCC_LENGTH, length(?MCC)).

-define(MCC_MNC, [
    "20214",
    "20412",
    "20414",
    "20415",
    "20416",
    "20417",
    "20418",
    "20420",
    "20421",
    "20423",
    "20424",
    "20428",
    "20468",
    "20469",
    "20498",
    "20620",
    "20811",
    "20813",
    "20814",
    "20815",
    "20816",
    "20817",
    "20820",
    "20821",
    "20822",
    "20823",
    "20824",
    "20825",
    "20826",
    "20827",
    "20828",
    "20829",
    "20831",
    "20888",
    "20889",
    "20891",
    "20892",
    "21411",
    "21415",
    "21416",
    "21417",
    "21418",
    "21419",
    "21420",
    "21421",
    "21422",
    "21423",
    "21425",
    "21426",
    "21427",
    "21432",
    "21630",
    "21670",
    "21671",
    "21890",
    "22230",
    "22233",
    "22234",
    "22235",
    "22243",
    "22244",
    "22248",
    "22277",
    "22288",
    "22299",
    "22611",
    "22616",
    "22812",
    "22851",
    "22852",
    "22853",
    "22854",
    "23099",
    "23115",
    "23199",
    "23211",
    "23212",
    "23213",
    "23214",
    "23215",
    "23217",
    "23219",
    "23411",
    "23412",
    "23414",
    "23415",
    "23416",
    "23417",
    "23418",
    "23419",
    "23420",
    "23422",
    "23423",
    "23424",
    "23425",
    "23426",
    "23427",
    "23428",
    "23430",
    "23431",
    "23432",
    "23433",
    "23434",
    "23435",
    "23436",
    "23437",
    "23450",
    "23451",
    "23455",
    "23457",
    "23458",
    "23475",
    "23476",
    "23477",
    "23478",
    "23491",
    "23492",
    "23494",
    "23812",
    "23820",
    "23823",
    "23828",
    "23830",
    "23877",
    "24011",
    "24012",
    "24013",
    "24014",
    "24015",
    "24016",
    "24017",
    "24018",
    "24019",
    "24020",
    "24022",
    "24023",
    "24024",
    "24025",
    "24026",
    "24027",
    "24028",
    "24029",
    "24030",
    "24035",
    "24036",
    "24212",
    "24214",
    "24217",
    "24220",
    "24221",
    "24222",
    "24223",
    "24411",
    "24412",
    "24413",
    "24414",
    "24421",
    "24426",
    "24482",
    "24491",
    "25011",
    "25012",
    "25013",
    "25015",
    "25016",
    "25017",
    "25019",
    "25020",
    "25028",
    "25035",
    "25039",
    "25044",
    "25092",
    "25093",
    "25099",
    "25521",
    "25539",
    "25550",
    "25567",
    "25568",
    "25999",
    "26011",
    "26012",
    "26013",
    "26014",
    "26015",
    "26016",
    "26017",
    "26018",
    "26034",
    "26035",
    "26036",
    "26038",
    "26098",
    "26211",
    "26212",
    "26213",
    "26214",
    "26216",
    "26217",
    "26220",
    "26242",
    "26243",
    "26277",
    "27077",
    "27099",
    "27211",
    "27213",
    "27411",
    "27821",
    "27877",
    "28020",
    "28967",
    "28968",
    "28988",
    "29340",
    "29341",
    "29364",
    "29370",
    "29475",
    "29577",
    "31011",
    "31012",
    "31013",
    "31014",
    "31015",
    "31016",
    "31020",
    "31023",
    "31024",
    "31025",
    "31026",
    "31026",
    "31030",
    "31031",
    "31032",
    "31033",
    "31034",
    "31038",
    "31040",
    "31046",
    "31050",
    "31060",
    "31070",
    "31080",
    "31090",
    "31120",
    "31130",
    "31140",
    "31150",
    "31170",
    "31180",
    "31190",
    "31230",
    "31240",
    "31290",
    "31611",
    "33011",
    "33420",
    "33430",
    "33440",
    "33450",
    "33450",
    "33460",
    "33470",
    "33480",
    "33490",
    "33820",
    "33850",
    "34011",
    "34012",
    "34020",
    "34250",
    "34430",
    "34650",
    "35099",
    "35230",
    "35250",
    "35650",
    "35670",
    "35830",
    "35850",
    "36050",
    "36070",
    "36251",
    "36269",
    "36291",
    "36295",
    "36320",
    "36430",
    "36439",
    "36620",
    "36650",
    "37412",
    "37650",
    "37650",
    "40177",
    "40211",
    "40217",
    "40277",
    "40411",
    "40412",
    "40413",
    "40414",
    "40415",
    "40416",
    "40417",
    "40418",
    "40419",
    "40422",
    "40424",
    "40425",
    "40428",
    "40429",
    "40430",
    "40433",
    "40434",
    "40436",
    "40438",
    "40441",
    "40442",
    "40444",
    "40445",
    "40450",
    "40451",
    "40452",
    "40453",
    "40454",
    "40455",
    "40456",
    "40457",
    "40458",
    "40459",
    "40460",
    "40462",
    "40464",
    "40466",
    "40467",
    "40468",
    "40469",
    "40470",
    "40471",
    "40472",
    "40473",
    "40474",
    "40475",
    "40476",
    "40477",
    "40478",
    "40479",
    "40480",
    "40481",
    "40482",
    "40483",
    "40485",
    "40486",
    "40487",
    "40488",
    "40489",
    "40534",
    "40553",
    "41220",
    "41230",
    "41240",
    "41250",
    "41280",
    "41288",
    "41532",
    "41533",
    "41534",
    "41535",
    "41536",
    "41537",
    "41538",
    "41539",
    "41677",
    "41820",
    "41830",
    "41840",
    "41845",
    "41882",
    "41892",
    "42512",
    "42514",
    "42515",
    "42516",
    "42519",
    "42577",
    "42888",
    "42891",
    "42898",
    "42899",
    "43211",
    "43214",
    "43219",
    "43220",
    "43232",
    "43235",
    "43270",
    "43612",
    "44011",
    "44012",
    "44013",
    "44014",
    "44015",
    "44016",
    "44017",
    "44018",
    "44019",
    "44020",
    "44021",
    "44022",
    "44023",
    "44024",
    "44025",
    "44026",
    "44027",
    "44028",
    "44029",
    "44030",
    "44031",
    "44032",
    "44033",
    "44034",
    "44035",
    "44036",
    "44037",
    "44038",
    "44039",
    "44040",
    "44041",
    "44042",
    "44043",
    "44044",
    "44045",
    "44046",
    "44047",
    "44048",
    "44049",
    "44050",
    "44051",
    "44052",
    "44053",
    "44054",
    "44055",
    "44056",
    "44058",
    "44060",
    "44061",
    "44062",
    "44063",
    "44064",
    "44065",
    "44066",
    "44067",
    "44068",
    "44069",
    "44070",
    "44071",
    "44072",
    "44073",
    "44074",
    "44075",
    "44076",
    "44077",
    "44078",
    "44079",
    "44080",
    "44081",
    "44082",
    "44083",
    "44084",
    "44085",
    "44086",
    "44087",
    "44088",
    "44089",
    "44090",
    "44092",
    "44093",
    "44094",
    "44095",
    "44096",
    "44097",
    "44098",
    "44099",
    "44140",
    "44141",
    "44142",
    "44143",
    "44144",
    "44145",
    "44161",
    "44162",
    "44163",
    "44164",
    "44165",
    "44170",
    "44190",
    "44191",
    "44192",
    "44193",
    "44194",
    "44198",
    "44199",
    "45411",
    "45412",
    "45413",
    "45414",
    "45415",
    "45416",
    "45417",
    "45418",
    "45419",
    "45420",
    "45428",
    "45429",
    "45440",
    "45447",
    "45618",
    "46611",
    "46656",
    "46668",
    "46688",
    "46689",
    "46692",
    "46693",
    "46697",
    "46699",
    "50211",
    "50212",
    "50213",
    "50216",
    "50217",
    "50218",
    "50219",
    "50220",
    "50511",
    "50512",
    "50513",
    "50514",
    "50516",
    "50519",
    "50524",
    "50526",
    "50571",
    "50572",
    "50588",
    "50590",
    "50599",
    "51011",
    "51021",
    "51027",
    "51028",
    "51089",
    "51099",
    "51518",
    "51588",
    "52015",
    "52018",
    "52020",
    "52023",
    "52099",
    "52512",
    "52811",
    "53024",
    "53028",
    "53943",
    "54411",
    "54715",
    "54720",
    "54927",
    "55280",
    "61820",
    "61925",
    "62120",
    "62125",
    "62130",
    "62140",
    "62150",
    "62160",
    "62199",
    "63086",
    "63088",
    "63089",
    "63090",
    "63415",
    "63422",
    "63513",
    "63514",
    "63719",
    "63730",
    "63760",
    "63771",
    "63782",
    "64011",
    "64111",
    "64114",
    "64118",
    "64122",
    "64130",
    "64133",
    "64166",
    "64282",
    "65512",
    "65519",
    "65521",
    "70267",
    "70268",
    "70830",
    "70840",
    "71021",
    "71030",
    "71073",
    "71220",
    "71420",
    "71615",
    "71617",
    "71620",
    "72220",
    "72270",
    "72411",
    "72412",
    "72415",
    "72416",
    "72419",
    "72423",
    "72424",
    "72430",
    "72431",
    "72432",
    "72433",
    "72434",
    "72437",
    "72438",
    "72439",
    "72454",
    "73011",
    "73012",
    "73013",
    "73014",
    "73015",
    "73019",
    "73220",
    "90111",
    "90112",
    "90113",
    "90114"
]).
-define(MCC_MNC_LENGTH, length(?MCC_MNC)).

-define(PATH_CT, "test/generated/ct/").
-define(PATH_EUNIT, "test/generated/eunit/").

-define(TIMETRAP_MINUTES, 15).

-endif.
