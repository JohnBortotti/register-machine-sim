type register = { label: string; mutable value: int}
type stack = { mutable s : int list }
type instruction = { opcode: string; func: machine -> machine }
and machine = { mutable pc: int; mutable registers: register list ; mutable stack: stack; memory: string array;}

exception InstructionNotFound of string

let rec get_register(label: string)(registers: register list) = 
    match registers with
    | [] -> None
    | hd :: tl -> if label = hd.label then Some(hd) else get_register label tl

let print_register_option = function None -> 0 | Some n -> n.value

let create_machine pc registers stack memory = {pc; registers; stack; memory}

let rec decode_instruction (opcode: string) (instructions: instruction list) = 
    match instructions with
    | [] -> raise (InstructionNotFound opcode)
    | hd :: tl -> if hd.opcode = opcode then hd else decode_instruction opcode tl

let rec execute_instructions (machine) (instructions: instruction list) = 
    let instruction = decode_instruction machine.memory.(machine.pc) instructions in
    Printf.printf "executing instructions: %s \n" instruction.opcode;
    execute_instructions (instruction.func machine) instructions

        (* Printf.printf "executing opcode %s \n" hd;
        let instruction = decode_instruction hd instructions in
        let machine_frame = instruction.func machine in
        execute_instructions machine_frame instructions *)

let first_machine = create_machine
    0
    [{ label="a"; value=3 }; { label="x"; value=2 }]
    {s=[]}
    [|"456"; "123"|]

let instructions_dictionary = [
        {opcode="123"; func=(function machine -> machine.pc <- machine.pc+1; machine )};
        {opcode="456"; func=(function machine -> machine.pc <- machine.pc+1; machine )}
    ]


let () =
    let final_machine = execute_instructions first_machine instructions_dictionary in
    Printf.printf "%d" final_machine.pc