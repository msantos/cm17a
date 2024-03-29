% Copyright (c) 2014-2022, Michael Santos <michael.santos@gmail.com>
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
    command/4,
    send/2,
    encode/3,
    insn/1
]).

-export([
    process/5, init/1, run/2, reset/2
]).

-define(UINT16(N), (N):2 / native - unsigned - integer - unit:8).
-define(UINT32(N), (N):4 / native - unsigned - integer - unit:8).

-define(TIOCMGET, serctl:constant(tiocmget)).
-define(TIOCMBIC, serctl:constant(tiocmbic)).
-define(TIOCMBIS, serctl:constant(tiocmbis)).
-define(TIOCM_DTR, serctl:constant(tiocm_dtr)).
-define(TIOCM_RTS, serctl:constant(tiocm_rts)).

-define(CM17A_SIGNAL_RESET, 0).
-define(CM17A_SIGNAL_SET_1, (?TIOCM_RTS)).
-define(CM17A_SIGNAL_SET_0, (?TIOCM_DTR)).
-define(CM17A_SIGNAL_STANDBY, (?TIOCM_RTS bor ?TIOCM_DTR)).

%% erlfmt-ignore
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

%% erlfmt-ignore
-define(CM17A_DEVICE, {
    {16#00, 16#00}, {16#00, 16#10}, {16#00, 16#08}, {16#00, 16#18}, % 1-4
    {16#00, 16#40}, {16#00, 16#50}, {16#00, 16#48}, {16#00, 16#58}, % 5-8
    {16#04, 16#00}, {16#04, 16#10}, {16#04, 16#08}, {16#04, 16#18}, % 9-12
    {16#04, 16#40}, {16#04, 16#50}, {16#04, 16#48}, {16#04, 16#58}  % 13-16
    }).

-type fd() :: any().

-type cm17a_data() :: <<_:40>>.
-type cm17a_cmd() ::
    on
    | off
    | bright
    | dim
    | all_off
    | all_on
    | lamps_off
    | lamps_on
    | pause.
-type cm17a_byte() ::
    16#00
    | 16#20
    | 16#88
    | 16#98
    | 16#80
    | 16#91
    | 16#84
    | 16#94
    | 16#20.

-type cm17a_code() :: 65..80.
-type cm17a_device() :: 1..16.
-type cm17a_insn() :: {non_neg_integer(), non_neg_integer(), timeout()}.

-export_type([cm17a_code/0, cm17a_cmd/0, cm17a_insn/0]).

-spec cmd(cm17a_cmd()) -> cm17a_byte().
cmd(on) -> 16#00;
cmd(off) -> 16#20;
cmd(bright) -> 16#88;
cmd(dim) -> 16#98;
cmd(all_off) -> 16#80;
cmd(all_on) -> 16#91;
cmd(lamps_off) -> 16#84;
cmd(lamps_on) -> 16#94;
cmd(pause) -> 16#20.

-define(DEVICE_LOOKUP(N, X), (element(N, (element(X, ?CM17A_DEVICE))))).

%% @doc Encode the CM17A command
-spec encode(cm17a_code(), cm17a_device(), cm17a_cmd()) -> cm17a_data().
encode(Code, Dev, Cmd) ->
    <<16#d5, 16#aa,
        ((element(Code - $A + 1, ?CM17A_HOUSECODE) bsl 4) bor
            (?DEVICE_LOOKUP(1, Dev))),
        (cmd(Cmd) bor (?DEVICE_LOOKUP(2, Dev))), 16#ad>>.

%% @doc Generate a representation of the state of the serial port for
%% each bit in the encoded command
%%
%% insn/1 calls into an NIF to retrieve constants for serctl:ioctl/3 so
%% the result is not portable.
-spec insn(cm17a_data()) -> [cm17a_insn()].
insn(Bytes) ->
    TIOCMBIC = ?TIOCMBIC,
    TIOCMBIS = ?TIOCMBIS,
    CM17A_SIGNAL_STANDBY = ?CM17A_SIGNAL_STANDBY,

    % DTR and RTS power the firecracker:
    %   * in standby, both DTR and RTS are set
    %   * to signal bit 0, RTS (logical 1) is cleared
    %   * to signal bit 1, DTR (logical 0) is cleared
    %
    % Since tuples start at offset 1, bit 0 maps to element 1 and
    % bit 1 maps to element 2.
    Signal = {?CM17A_SIGNAL_SET_1, ?CM17A_SIGNAL_SET_0},

    Insn = [
        [
            {TIOCMBIC, element(N + 1, Signal), 1},
            {TIOCMBIS, CM17A_SIGNAL_STANDBY, 0}
        ]
     || <<N:1>> <= Bytes
    ],
    lists:flatten([
        {TIOCMBIS, CM17A_SIGNAL_STANDBY, 350},
        Insn,
        {TIOCMBIS, CM17A_SIGNAL_STANDBY, 350}
    ]).

%% @doc Send a command to the X10 firecracker controller
-spec command(iodata(), cm17a_code(), cm17a_device(), cm17a_cmd()) ->
    ok | {error, file:posix()}.
command(Serial, Code, Dev, Cmd) ->
    case serctl:open(Serial) of
        {ok, FD} ->
            Bytes = encode(Code, Dev, Cmd),
            Insn = insn(Bytes),
            Result = send(FD, Insn),
            serctl:close(FD),
            Result;
        Error ->
            Error
    end.

%% @doc Perform actions in the list of instructions on a serial port
%%
%% ```
%% {ok,FD} = serctl:open("/dev/ttyUSB0"),
%% cm17a:send(FD, cm17a:insn(cm17a:encode($A,1,on))).
%% '''
%%
%% Is equivalent to:
%% ```
%% cm17a:command("/dev/ttyUSB0", $A, 1, on).
%% '''
-spec send(fd(), [cm17a_insn()]) -> 'ok' | {'error', file:posix()}.
send(FD, Insn) ->
    process(FD, Insn, fun init/1, fun run/2, fun reset/2).

-spec init(fd()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
init(FD) ->
    case serctl:ioctl(FD, ?TIOCMGET, <<0:32>>) of
        {ok, Status0} ->
            Status1 =
                binary:decode_unsigned(
                    Status0,
                    erlang:system_info(endian)
                ) band (?TIOCM_DTR bor ?TIOCM_RTS),
            {ok, Status1 bxor (?TIOCM_DTR bor ?TIOCM_RTS)};
        Error ->
            Error
    end.

-spec run(fd(), [cm17a_insn()]) -> 'ok' | {'error', file:posix()}.
run(_FD, []) ->
    ok;
run(FD, [{Request, Arg, Delay} | Insn]) ->
    case serctl:ioctl(FD, Request, <<?UINT32(Arg)>>) of
        {ok, _} ->
            timer:sleep(Delay),
            run(FD, Insn);
        Error ->
            Error
    end.

-spec reset(fd(), non_neg_integer()) -> 'ok' | {'error', file:posix()}.
reset(FD, Status) ->
    case serctl:ioctl(FD, ?TIOCMBIC, <<?UINT32(Status)>>) of
        {ok, _} -> ok;
        Error -> Error
    end.

-spec process(
    FD :: fd(),
    Insn :: [cm17a_insn()],
    Init :: fun((any()) -> {ok, non_neg_integer()} | {error, file:posix()}),
    Run :: fun((any(), [cm17a_insn()]) -> ok | {error, file:posix()}),
    Reset :: fun((any(), non_neg_integer()) -> ok | {error, file:posix()})
) ->
    ok.
process(FD, Insn, Init, Run, Reset) ->
    case Init(FD) of
        {ok, State} ->
            Reply = Run(FD, Insn),
            Reset(FD, State),
            Reply;
        Error ->
            Error
    end.
