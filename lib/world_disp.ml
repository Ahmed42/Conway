
open Graphics
open World

(* let win_height = 700 *)
(* let win_width = 700 *)
let cell_height = 15
let cell_width = 15
let x_gap = 2
let y_gap = 2

let orange = rgb 230 190 50

let init_display () = 
  open_graph "";
  set_color black;
  fill_rect 0 0 (size_x () - 1) (size_y () - 1)

let term_display () =
  close_graph ()

let display_cell cell row col total_rows x_offset y_offset =
  let cell_color = 
    match cell with
    | Dead -> black
    | Alive -> orange
  in let x = x_offset + col * (x_gap + cell_width)
  in let y = y_offset + (total_rows - row - 1) * (y_gap + cell_height)
  in
  (set_color cell_color;
   fill_rect x y cell_width cell_height)


let display_world world =
  let win_width, win_height = size_x (), size_y ()
  in let total_rows = Array.length world 
  in let total_cols = Array.length world.(0) (* Assumes that world has > 0 cells *)
  in let world_width = x_gap * (total_cols - 1) + cell_width * total_cols
  in let world_height = y_gap * (total_rows - 1) + cell_height * total_rows
  in let x_offset = (win_width - world_width)/2
  in let y_offset = (win_height - world_height)/2
  in
  begin
    set_color white;
    draw_rect 
      (x_offset - x_gap) (y_offset - y_gap) 
      (world_width + 2*x_gap) (world_height + 2*y_gap);
    Array.iteri (fun row_index row ->
        Array.iteri (fun col_index cell ->
            display_cell cell row_index col_index total_rows x_offset y_offset) row) world
end

let within_bounds (x, y) world =
  let win_width, win_height = size_x (), size_y ()
  in let total_rows = Array.length world 
  in let total_cols = Array.length world.(0) (* Assumes that world has > 0 cells *)
  in let world_width = x_gap * (total_cols - 1) + cell_width * total_cols
  in let world_height = y_gap * (total_rows - 1) + cell_height * total_rows
  in let x_offset = (win_width - world_width)/2
  in let y_offset = (win_height - world_height)/2
  in
        x > x_offset && y > y_offset && 
        x < (x_offset + world_width) && y < (y_offset + world_height)

let xy_to_colrow (x, y) world =
  let win_width, win_height = size_x (), size_y ()
  in let total_rows = Array.length world 
  in let total_cols = Array.length world.(0) (* Assumes that world has > 0 cells *)
  in let world_width = x_gap * (total_cols - 1) + cell_width * total_cols
  in let world_height = y_gap * (total_rows - 1) + cell_height * total_rows
  in let x_offset = (win_width - world_width)/2
  in let y_offset = (win_height - world_height)/2
  in 
  ((x - x_offset) / (x_gap + cell_width)),
  -((y - y_offset) / (y_gap + cell_height) - total_rows + 1)

type sim_state = Running | Paused


let run steps =
  (* initialize world and graphics *)
  init_display ();
  let world = ConwayWorld.create 50 Dead in
  (* event loop *)
  let rec loop world world_state mouse_down last_update_time steps =
    (*         check events *)
    if button_down () then
      loop world world_state true last_update_time steps
    else if mouse_down then
      let xy = mouse_pos () in
      let updated_world =
        if within_bounds xy world then
          let col, row = xy_to_colrow xy world in
          ConwayWorld.flip_cell world row col
        else
          world
      in
      loop updated_world world_state false last_update_time steps
    else if key_pressed () then
      let key = read_key () in
      match key with
      | 'p' | 'P' -> loop world Paused mouse_down last_update_time steps
      | 'r' | 'R' -> loop world Running mouse_down last_update_time steps
      | 'q' | 'Q' -> ()
      | 'c' | 'C' -> 
                      let new_world = ConwayWorld.create 50 Dead in
                      loop new_world Paused false 0.0 steps
      | _ -> 
        begin 
          print_char key; 
          loop world world_state mouse_down last_update_time steps 
        end 
    else
      let updated_world, last_update_time =
        match world_state with
        | Running -> 
          let time_now = Sys.time () in
          let time_diff = time_now -. last_update_time in
          let required_time_diff = 1.0 /. (float_of_int steps) in
          if time_diff > required_time_diff then
            (ConwayWorld.n_steps 1 world), time_now
          else world, last_update_time
        | Paused -> world, last_update_time
      in
      begin
        display_world updated_world; 
        loop updated_world world_state mouse_down last_update_time steps
      end
  in
  begin
    loop world Paused false 0.0 steps;
    term_display () 
  end
