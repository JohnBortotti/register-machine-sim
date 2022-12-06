open Register_machine_sim

let first_machine = create_machine
    0
    [{ label="a"; value=3 }; { label="x"; value=2 }]
    {s=[]}
    [|"456"; "123"|]

let instructions_dictionary = [
        {opcode="123"; registers_operand=[||]; func=(function machine -> machine.pc <- machine.pc+1; machine )};
        {opcode="456"; registers_operand=[||]; func=(function machine -> machine.pc <- machine.pc+1; machine )}
    ]
    
let () =
    let final_machine = execute_instructions first_machine instructions_dictionary in
    Printf.printf "%d" final_machine.pc