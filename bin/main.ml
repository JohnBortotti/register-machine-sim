open Register_machine_sim

(* [] create stack operations *)
(* [] add operands evaluation *)
let first_machine = create_machine
    0
    [|{value=0 }; {value=0}|]
    {s=[]}
    [|"rprt 0"; "radd 0 1"; "rprt 0"|]

let instructions_dictionary = [
        {opcode="rprt"; func=(
            function machine -> 
                machine.pc <- machine.pc+1; 
                Printf.printf "%d \n" machine.registers.(0).value;
                machine 
        )};
        {opcode="radd"; func=(
            function machine -> 
                machine.pc <- machine.pc+1; 
                machine.registers.(0).value <- machine.registers.(0).value + 1;
                machine 
        )}
    ]
    
let () =
    let final_machine = execute_instructions first_machine instructions_dictionary in
    Printf.printf "%d" final_machine.pc