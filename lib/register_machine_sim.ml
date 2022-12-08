module Stack = struct
    type t = { mutable s : int list }

    exception EmptyStack

    let create() = {s = []}

    let pop s = 
        match s.s with
        | [] -> raise EmptyStack
        | hd::tl -> s.s <- tl; hd

    let push x s = s.s <- x :: s.s 
end

module Machine = struct
    type register = {mutable value: int}
    type operands = int array
    type memory_instruction = {opcode: string; operands: operands}
    type instruction_signature = {opcode: string; func: (machine * operands) -> machine}
    and machine = {mutable pc: int; mutable registers: register array ; mutable stack: Stack.t; memory: memory_instruction array;}

    exception InstructionNotFound of string
    
    let create_machine registers memory = {pc=0; registers; stack=(Stack.create)(); memory}

    let rec decode_instruction (memory_instruction: memory_instruction) (instructions: instruction_signature list) =
        let opcode =  memory_instruction.opcode in
        match instructions with
        | [] -> raise (InstructionNotFound opcode)
        | hd :: tl -> if hd.opcode = opcode then (hd, memory_instruction.operands) else decode_instruction memory_instruction tl
        

    let rec execute_instructions (machine) (instructions: instruction_signature list) =
        let instruction , operands = decode_instruction machine.memory.(machine.pc) instructions in
        Printf.printf "executing instruction: %s \n" instruction.opcode;
        execute_instructions (instruction.func (machine, operands)) instructions
end