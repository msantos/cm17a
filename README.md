# Erlang X10 Firecracker (CM17A) Interface

An Erlang interface to the X10 Firecracker (CM17A) home automation
RF transmitters.

# OVERVIEW

* Switch on a device attached to A1 using /dev/ttyUSB0

~~~erlang
cm17a:command("/dev/ttyUSB0", $A, 1, on).
~~~

* Generate and save the list of instructions for toggling the serial port

~~~erlang
On = cm17a:insn(cm17a:encode($A, 1, on)),
Off = cm17a:insn(cm17a:encode($A, 1, off)),

{ok, FD} = serctl:open("/dev/ttyUSB0"),
ok = cm17a:send(FD, On),
ok = cm17a:send(FD, Off).
~~~

# DATA TYPES

    fd()

        An NIF resource returned from serctl:open/2.

    cm17a_code = 65..80

        The CM17A house code. Valid values are in the range $A to $P.

    cm17a_device = 1..16

        The CM17A device code.
        
    cm17a_cmd = on
        | off
        | bright
        | dim
        | all_off
        | all_on
        | lamps_off
        | lamps_on
        | pause

    cm17a_insn = {Ioctl :: non_neg_integer(), Arg :: non_neg_integer(),
        Delay :: Timer:timeout()}

        An instruction for the serial port.

# EXPORTS

    command(SerialPort, Housecode, Device, Command) -> ok | {error,posix()}

        Types   SerialPort = iodata()
                Housecode = cm17a_code()
                Device = cm17a_device()
                Command = cm17a_cmd()

        Send a command to the X10 firecracker controller.

    encode(Housecode, Device, Command) -> binary()

        Types   Housecode = cm17a_code()
                Device = cm17a_device()
                Command = cm17a_cmd()

        Encodes the CM17A command.

    insn(Bytes) -> Insn

        Types   Insn = [cm17a_insn()]

        Generates a representation of the state of the serial port for
        each bit in the encoded command.

        insn/1 calls into an NIF to retrieve constants for serctl:ioctl/3
        so the result is not portable.

    send(FD, Insn) -> ok | {error,posix()}

        Types   FD = fd()        
                Insn = [cmd17a_insn()]

        Performs the actions in the list of instructions on the serial
        port.

            {ok,FD} = serctl:open("/dev/ttyUSB0"), 
            cm17a:send(FD, cm17a:insn(cm17a:encode($A,1,on))).

        Is equivalent to:

            cm17a:command("/dev/ttyUSB0", $A, 1, on).

# REFERENCES

* CM17A protocol

    http://www.linuxha.com/common/cm17a.html

* bottlerocket

    http://www.linuxha.com/bottlerocket/

* flipit

    https://code.google.com/p/flipit/
