% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(cm17a).

-export([
        command/4, send/2,
        encode/3, insn/1
    ]).

-export([
        process/5, init/1, run/2, reset/2
    ]).

-define(UINT16(N), (N):2/native-unsigned-integer-unit:8).
-define(UINT32(N), (N):4/native-unsigned-integer-unit:8).

-define(TIOCMGET, serctl:constant(tiocmget)).
-define(TIOCMBIC, serctl:constant(tiocmbic)).
-define(TIOCMBIS, serctl:constant(tiocmbis)).
-define(TIOCM_DTR, serctl:constant(tiocm_dtr)).
-define(TIOCM_RTS, serctl:constant(tiocm_rts)).

-define(CM17A_SIGNAL_RESET, 0).
-define(CM17A_SIGNAL_SET_1, (?TIOCM_RTS)).
-define(CM17A_SIGNAL_SET_0, (?TIOCM_DTR)).
-define(CM17A_SIGNAL_STANDBY, (?TIOCM_RTS bor ?TIOCM_DTR)).

-define(CM17A_HOUSECODE, {
        16#06, % A
        16#07, % B
        16#04, % C
        16#05, % D
        16#08, % E
        16#09, % F
        16#0a, % G
        16#0b, % H
        16#0e, % I
        16#0f, % J
        16#0c, % K
        16#0d, % L
        16#00, % M
        16#01, % N
        16#02, % O
        16#03  % P
    }).

-define(CM17A_DEVICE, {
    {16#00, 16#00}, {16#00, 16#10}, {16#00, 16#08}, {16#00, 16#18}, % 1-4
    {16#00, 16#40}, {16#00, 16#50}, {16#00, 16#48}, {16#00, 16#58}, % 5-8
    {16#04, 16#00}, {16#04, 16#10}, {16#04, 16#08}, {16#04, 16#18}, % 9-12
    {16#04, 16#40}, {16#04, 16#50}, {16#04, 16#48}, {16#04, 16#58}  % 13-16
    }).

-type fd() :: any().

-type cm17a_cmd() ::
    on | off | bright | dim |
    all_off | all_on | lamps_off | lamps_on | pause.

-type cm17a_code() :: 65..80.
-type cm17a_device() :: 1..16.
-type cm17a_insn() :: {non_neg_integer(), non_neg_integer(), timer:timeout()}.

-export_type([cm17a_code/0, cm17a_cmd/0, cm17a_insn/0]).

-spec cmd(cm17a_cmd()) -> byte().
cmd(on) -> 16#00;
cmd(off) -> 16#20;
cmd(bright) -> 16#88;
cmd(dim) -> 16#98;
cmd(all_off) -> 16#80;
cmd(all_on) -> 16#91;
cmd(lamps_off) -> 16#84;
cmd(lamps_on) -> 16#94;
cmd(pause) -> 16#20.

-spec encode(cm17a_code(),cm17a_device(),cm17a_cmd()) -> binary().
encode(Code, Dev, Cmd) ->
    <<16#d5, 16#aa,
        ((element(Code-$A+1, ?CM17A_HOUSECODE) bsl 4)
            bor (element(1, element(Dev, ?CM17A_DEVICE)))),
        (cmd(Cmd) bor (element(2, element(Dev, ?CM17A_DEVICE)))),
        16#ad>>.

-spec insn(binary()) -> [cm17a_insn()].
insn(Bytes) ->
    TIOCMBIC = ?TIOCMBIC,
    TIOCMBIS = ?TIOCMBIS,
    CM17A_SIGNAL_SET_1 = ?CM17A_SIGNAL_SET_1,
    CM17A_SIGNAL_SET_0 = ?CM17A_SIGNAL_SET_0,
    CM17A_SIGNAL_STANDBY = ?CM17A_SIGNAL_STANDBY,

    Insn = [ case N of
        0 -> [{TIOCMBIC, CM17A_SIGNAL_SET_1, 1},
                {TIOCMBIS, CM17A_SIGNAL_STANDBY, 0}];
        1 -> [{TIOCMBIC, CM17A_SIGNAL_SET_0, 1},
                {TIOCMBIS, CM17A_SIGNAL_STANDBY, 0}]
    end || <<N:1>> <= Bytes ],
    lists:flatten([
            {TIOCMBIS, CM17A_SIGNAL_STANDBY, 350},
            Insn,
            {TIOCMBIS, CM17A_SIGNAL_STANDBY, 350}
        ]).

-spec command(iodata(),cm17a_code(),cm17a_device(),cm17a_cmd()) -> 'ok'.
command(Serial, Code, Dev, Cmd) ->
    {ok,FD} = serctl:open(Serial),

    Bytes = encode(Code, Dev, Cmd),
    Insn = insn(Bytes),

    send(FD, Insn),

    serctl:close(FD).

-spec send(fd(),[cm17a_insn()]) -> 'ok'.
send(FD, Insn) ->
    process(FD, Insn, fun init/1, fun run/2, fun reset/2).

-spec init(fd()) -> non_neg_integer().
init(FD) ->
    {ok, Status0} = serctl:ioctl(FD, ?TIOCMGET, <<0:32>>),
    Status1 = binary:decode_unsigned(Status0, erlang:system_info(endian))
        band (?TIOCM_DTR bor ?TIOCM_RTS),
    Status1 bxor (?TIOCM_DTR bor ?TIOCM_RTS).

-spec run(fd(),[cm17a_insn()]) -> 'ok'.
run(FD, Insn) ->
    [ begin
        case serctl:ioctl(FD, Request, <<?UINT32(Arg)>>) of
            {ok,_} -> timer:sleep(Delay);
            Error -> error(Error, [Request, Arg, Delay])
        end
      end || {Request, Arg, Delay} <- Insn ],
  ok.

-spec reset(fd(),non_neg_integer()) -> 'ok'.
reset(FD, Status) ->
    {ok,_} = serctl:ioctl(FD, ?TIOCMBIC, <<?UINT32(Status)>>),
    ok.

-spec process(FD :: fd(), Insn :: [cm17a_insn()],
    Init :: fun((any()) -> non_neg_integer()),
    Run :: fun((any(), [cm17a_insn()]) -> ok),
    Reset :: fun((any(),non_neg_integer()) -> ok)) -> ok.
process(FD, Insn, Init, Run, Reset) ->
    State = Init(FD),
    Run(FD, Insn),
    Reset(FD, State).
