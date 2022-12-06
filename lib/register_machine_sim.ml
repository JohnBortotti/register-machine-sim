type register = { label: string; mutable value: int}
type stack = { mutable s : int list }
type instruction = { opcode: string; registers_operand: string array; func: machine -> machine }
and machine = { mutable pc: int; mutable registers: register list ; mutable stack: stack; memory: string array;}

exception InstructionNotFound of string

let create_machine pc registers stack memory = {pc; registers; stack; memory}

let rec decode_instruction (opcode: string) (instructions: instruction list) = 
    match instructions with
    | [] -> raise (InstructionNotFound opcode)
    | hd :: tl -> if hd.opcode = opcode then hd else decode_instruction opcode tl

let rec execute_instructions (machine) (instructions: instruction list) = 
    let instruction = decode_instruction machine.memory.(machine.pc) instructions in
    Printf.printf "executing instruction: %s \n" instruction.opcode;
    execute_instructions (instruction.func machine) instructions