open Conway

let steps = int_of_string Sys.argv.(1) 
let () = World_disp.run steps
