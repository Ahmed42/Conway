

type cell_state = Dead | Alive
type t = cell_state array array

module type MinWorld = sig
        val one_step_cell: (t -> int -> int -> int) -> 
                t -> int -> int -> cell_state
end

module type CompleteWorld = sig
        val create: int -> cell_state -> t
(*         val count_alive: t -> int -> int -> int *)
(*         val one_step_cell: t -> int -> int -> cell_state *)
(*         val one_step: t -> t *)
        val n_steps: int -> t -> t
        val set_cell: t -> int -> int -> cell_state -> t
        val flip_cell: t -> int -> int -> t 
end

module Make (M : MinWorld) : CompleteWorld = struct
        let create n cell =
                Array.make_matrix n n cell

        let count_alive world row col =
                let len = Array.length world in
                let ( +% ) a b = (a + b) mod len in
                let ( -% ) a b = 
                        let result = (a - b) mod len 
                        in if result >= 0 then result
                        else result + len
                in
                let neighborhood = [
                        world.(row-%1).(col-%1);
                        world.(row).(col-%1);
                        world.(row+%1).(col-%1);
                        world.(row-%1).(col);
                        world.(row+%1).(col);
                        world.(row-%1).(col+%1);
                        world.(row).(col+%1);
                        world.(row+%1).(col+%1)
                ] in
                List.fold_left 
                (fun acc cell -> match cell with Dead -> acc | Alive -> 1 + acc) 
                0 neighborhood

        let one_step_cell = M.one_step_cell count_alive

        let one_step world =
                Array.mapi 
                (fun row_index row ->
                        Array.mapi (fun col_index _ -> 
                                one_step_cell world row_index col_index 
                        ) row 
                ) world


        let rec n_steps n world =
                match n with
                | 0 -> world
                | _ -> n_steps (n-1) (one_step world) 

        let set_cell world row_i col_i state =
          Array.mapi 
            (fun row_index row ->
              if row_index = row_i then
                Array.mapi 
                  (fun col_index col -> 
                    if col_index = col_i then state
                    else col) 
                  row
              else row) world

        let flip_cell world row_i col_i =
          let cell = world.(row_i).(col_i) in
          match cell with
          | Alive -> set_cell world row_i col_i Dead
          | Dead -> set_cell world row_i col_i Alive 
end

module ConwayWorld = Make(struct
        let one_step_cell count_alive world row_index col_index =
                let alive_neighbors = count_alive world row_index col_index in
                let cell = world.(row_index).(col_index) in

                match (cell, alive_neighbors) with
                | (_, 3) -> Alive
                | (Alive, 2) -> Alive
                | _ -> Dead
end)





