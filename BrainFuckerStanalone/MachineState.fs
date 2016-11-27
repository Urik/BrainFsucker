module MachineState
open Instructions

type MachineState = {
    tape: byte list;
    position: int;
    instructions: Instruction list;
    input: byte seq;
}

let create (instructions, input) =
    {
        tape = [ for i in 0 .. 300000 -> 0uy ];
        position = 0;
        instructions = instructions
        input = input
    }