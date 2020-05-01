-module(ntp).
-export([get_time/1, get_time/0]).

-define(NTP_PORT,       123).                   % udp
-define(SERVER_TIMEOUT, 5000).                  % ms
-define(EPOCH,          2208988800).            % offset yr 1900 to unix epoch


ntp_servers() ->
  [ "0.europe.pool.ntp.org",
    "1.europe.pool.ntp.org",
    "2.europe.pool.ntp.org" ].

get_time() ->
  Random_server = lists:nth(rand:uniform(length(ntp_servers())), ntp_servers()),
  get_time(Random_server).

get_time(Host) ->
  Resp = ntp_request(Host, create_ntp_request()),
  process_ntp_response(Resp) .

ntp_request(Host, Binary) ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  gen_udp:send(Socket, Host, ?NTP_PORT, Binary),
  {ok, {_Address, _Port, Resp}} = gen_udp:recv(Socket, 0, 500),
  gen_udp:close(Socket),
  Resp.

process_ntp_response(Ntp_response) ->
  << LI:2, Version:3, Mode:3, Stratum:8, Poll:8/signed, Precision:8/signed,
     RootDel:32, RootDisp:32, R1:8, R2:8, R3:8, R4:8, RtsI:32, RtsF:32,
     OtsI:32, OtsF:32,   RcvI:32, RcvF:32, XmtI:32, XmtF:32 >> = Ntp_response,
  {NowMS, NowS, NowUS} = erlang:timestamp(),
  NowTimestamp = NowMS * 1.0e6 + NowS + NowUS/1000,
  TransmitTimestamp = XmtI - ?EPOCH + binfrac(XmtF),
  { {li, LI}, {vn, Version}, {mode, Mode}, {stratum, Stratum}, {poll, Poll}, {precision, Precision},
    {rootDelay, RootDel}, {rootDispersion, RootDisp}, {referenceId, R1, R2, R3, R4},
    {referenceTimestamp, RtsI - ?EPOCH + binfrac(RtsF)},
    {originateTimestamp, OtsI - ?EPOCH + binfrac(OtsF)},
    {receiveTimestamp,   RcvI - ?EPOCH + binfrac(RcvF)},
    {transmitTimestamp,  TransmitTimestamp},
    {clientReceiveTimestamp, NowTimestamp},
    {offset, TransmitTimestamp - NowTimestamp} }.

create_ntp_request() ->
  << 0:2, 4:3, 3:3,  0:(3*8 + 3*32 + 4*64) >>.

binfrac(Bin) ->
  binfrac(Bin, 2, 0).
binfrac(0, _, Frac) ->
  Frac;
binfrac(Bin, N, Frac) ->
  binfrac(Bin bsr 1, N*2, Frac + (Bin band 1)/N).
