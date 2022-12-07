type register = { mutable value: int }
type stack = { mutable s : int list }

type instruction = { opcode: string; func: machine -> machine }
and machine = { mutable pc: int; mutable registers: register array ; mutable stack: stack; memory: string array;}

exception InstructionNotFound of string
exception RegisterNotFound of string
    
let create_machine pc registers stack memory = {pc; registers; stack; memory}

let rec decode_instruction (opcode: string) (instructions: instruction list) = 
    let sub_opcode = String.sub opcode 0 4  in
    match instructions with
    | [] -> raise (InstructionNotFound opcode)
    | hd :: tl -> if hd.opcode = sub_opcode then hd else decode_instruction opcode tl

let rec execute_instructions (machine) (instructions: instruction list) =
    let instruction = decode_instruction machine.memory.(machine.pc) instructions in
    Printf.printf "executing instruction: %s \n" instruction.opcode;
    execute_instructions (instruction.func machine) instructions