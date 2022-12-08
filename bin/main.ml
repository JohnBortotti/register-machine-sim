open Register_machine_sim

let first_machine = Machine.create_machine
    [|{value=0 }; {value=0}|]
    [|
        {opcode="rprt"; operands=[|1|]}; 
        {opcode="radd"; operands=[|1|]}; 
        {opcode="rprt"; operands=[|1|]};
        {opcode="stckp"; operands=[|3|]};
        {opcode="stckp"; operands=[|6|]};
        {opcode="stckp"; operands=[|2|]};
        {opcode="stckd"; operands=[||]};
        {opcode="stckd"; operands=[||]};
    |]

let instructions_dictionary: Machine.instruction_signature list = [
        {
            opcode="rprt";
            func=(
                function (machine, operands) -> 
                    machine.pc <- machine.pc+1; 
                    Printf.printf "%d \n" machine.registers.(operands.(0)).value;
                    machine 
            )
        };
        {
            opcode="radd"; 
            func=(
                function (machine, operands) -> 
                    machine.pc <- machine.pc+1; 
                    machine.registers.(operands.(0)).value <- machine.registers.(operands.(0)).value + 1;
                    machine 
            )
        };
        {
            opcode="stckp"; 
            func=(
                function (machine, operands) -> 
                    machine.pc <- machine.pc+1;
                    Stack.push operands.(0) machine.stack;
                    machine
            )
            
        };
        {
            opcode="stckd"; 
            func=(
                function (machine, _) -> 
                    machine.pc <- machine.pc+1;
                    Printf.printf "removed from stack: %d \n" (Stack.pop machine.stack);
                    machine
            )
            
        };
]
    
let () = Machine.execute_instructions first_machine instructions_dictionary