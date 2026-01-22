open Graphics
open Game

(* ---------- Configuration ---------- *)

let window_width  = 690
let window_height = 500
let cell_size  = 30

let border_offset = 10
let end_grid_offset = (10 * cell_size + border_offset + 30)



let colors = [|
  white;
  black;
  red;
  green;
  blue;
  yellow;
  cyan;
  magenta;
|]

type rect = { x : int; y : int; w : int; h : int }

let contains r mx my =
  mx >= r.x && mx <= r.x + r.w &&
  my >= r.y && my <= r.y + r.h

type button = {
  rect : rect;
  label : string;
  action : unit -> unit;
}

let draw_button b =
  set_color (rgb 220 220 220);
  fill_rect b.rect.x b.rect.y b.rect.w b.rect.h;
  set_color black;
  draw_rect b.rect.x b.rect.y b.rect.w b.rect.h;

  moveto
    (b.rect.x + b.rect.w / 2 - 4*(String.length (b.label)))
    (b.rect.y + b.rect.h / 2 - 11);
  draw_string b.label

let handle_button b mx my =
  if contains b.rect mx my then
    b.action ()


(* ---------- Grid ---------- *)

let rows = ref 5
let cols = ref 4

(* Grid stores color indices *)
let make_grid r c = Array.make_matrix r c 0

let start_grid = ref (make_grid !rows !cols)
let end_grid = ref (make_grid !rows !cols)

let grid_to_string g = 
  let res = ref "" in
  for r = !rows - 1 downto 0 do
    for c = 0 to !cols - 1 do
      res:= !res ^ (
          if g.(r).(c) = 0 then  "_"
          else if g.(r).(c) = 1 then "X"
          else String.make 1 (Char.chr (64 - 1 + g.(r).(c)))
        )
    done;
    res := (!res) ^ "\n";
  done;
  !res



(* ---------- Drawing ---------- *)

let draw_cell_start r c =
  let x = c * cell_size + border_offset in
  let y = r * cell_size + border_offset in
  set_color colors.(!start_grid.(r).(c));
  fill_rect x y cell_size cell_size;
  set_color black;
  draw_rect x y cell_size cell_size

let draw_cell_end r c =
  let x = end_grid_offset + c * cell_size + border_offset in
  let y = r * cell_size + border_offset in
  set_color colors.(!end_grid.(r).(c));
  fill_rect x y cell_size cell_size;
  set_color black;
  draw_rect x y cell_size cell_size


let draw_grid_start () =
  set_color white;
  fill_rect 0 0 (10 * cell_size + border_offset) (10 * cell_size + border_offset);
  set_color black;
  for r = 0 to !rows - 1 do
    for c = 0 to !cols - 1 do
      draw_cell_start r c
    done
  done

let draw_grid_end () =
  set_color white;
  fill_rect end_grid_offset 0 (10 * cell_size + border_offset) (10 * cell_size + border_offset);
  set_color black;
  for r = 0 to !rows - 1 do
    for c = 0 to !cols - 1 do
      draw_cell_end r c
    done
  done

let draw_grids () = (draw_grid_start () ; draw_grid_end ())

let clear_button =
  {
    rect = { x = 30; y = window_height-100; w = 80; h = 30 };
    label = "Clear";
    action =
      (fun () ->
        for r = 0 to !rows - 1 do
          for c = 0 to !cols - 1 do
            !start_grid.(r).(c) <- 0;
            !end_grid.(r).(c) <- 0
          done
        done;
        draw_grids ()
      );
  }

let incr_cols_button = 
  {
    rect = { x = 200; y = window_height-100; w = 30; h = 30 };
    label = "+";
    action =
      (fun () ->
        if !cols < 10 then incr cols;
        start_grid := make_grid !rows !cols;
        end_grid := make_grid !rows !cols;
        draw_grids ()
      );
  }
let decr_cols_button = 
  {
    rect = { x = 250; y = window_height-100; w = 30; h = 30 };
    label = "-";
    action =
      (fun () ->
        if !cols > 1 then decr cols;
        start_grid := make_grid !rows !cols;
        end_grid := make_grid !rows !cols;
        draw_grids ()
      );
  }


let incr_rows_button = 
  {
    rect = { x = 350; y = window_height-100; w = 30; h = 30 };
    label = "+";
    action =
      (fun () ->
        if !rows <10 then incr rows;
        start_grid := make_grid !rows !cols;
        end_grid := make_grid !rows !cols;
        draw_grids ()
      );
  }
let decr_rows_button = 
  {
    rect = { x = 400; y = window_height-100; w = 30; h = 30 };
    label = "-";
    action =
      (fun () ->
        if !rows > 1 then decr rows;
        start_grid := make_grid !rows !cols;
        end_grid := make_grid !rows !cols;
        draw_grids ()
      );
  }

let save_button = 
  {
    rect = { x = 30; y = window_height-140; w = 80; h = 30 };
    label = "Save";
    action =
      (fun () ->
        (* grid_to_string (!start_grid) *)
        Game.write_file "start.txt" (grid_to_string (!start_grid));
        Game.write_file "end.txt" (grid_to_string (!end_grid));
      );
  }

let quit_button =
  {
    rect = { x = 30; y = window_height-180; w = 80; h = 30 };
    label = "Solve";
    action = (fun () ->
      close_graph ();
      exit 0
    );
  }


(* ---------- Mouse handling ---------- *)

let cell_from_mouse x y =
  let cs = (x- border_offset) / cell_size in
  let ce = (x-end_grid_offset-border_offset) / cell_size in
  let r = (y-border_offset) / cell_size in
  if r >= 0 && r < !rows && cs >= 0 && cs < !cols then
    Some (0,r, cs)
  else if r >= 0 && r < !rows && ce >= 0 && ce < !cols then
    Some (1,r, ce)
  else
    None

let cycle_color_start r c =	
  (!start_grid).(r).(c) <- ((!start_grid).(r).(c) + 1) mod Array.length colors

let cycle_color_end r c =	
  (!end_grid).(r).(c) <- ((!end_grid).(r).(c) + 1) mod Array.length colors

(* let clear_cell r c g =
  !g.(r).(c) <- 0 *)

(* ---------- Main loop ---------- *)

let rec loop () =
  let e = wait_next_event [Button_down] in
  let mx = e.mouse_x and my = e.mouse_y in

  handle_button clear_button mx my;

  handle_button incr_rows_button mx my;
  handle_button decr_rows_button mx my;
  handle_button incr_cols_button mx my;
  handle_button decr_cols_button mx my;
  handle_button save_button mx my;
  handle_button quit_button mx my;

  match cell_from_mouse mx my with
  | Some (i,r, c) when i=0 ->
      if e.button then cycle_color_start r c;
      draw_cell_start r c;
      loop ()
  | Some (i,r, c) when i=1 ->
      if e.button then cycle_color_end r c;
      draw_cell_end r c;
      loop ()
  | _ -> loop ()


(* ---------- Entry point ---------- *)

let () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "Klotski";
  auto_synchronize true;
  draw_grids ();

  moveto
    (200)
    (window_height-70);
  draw_string "Columns";

  moveto
    (365)
    (window_height-70);
  draw_string "Rows";

  draw_button  clear_button;
  draw_button  incr_rows_button;
  draw_button  decr_rows_button;
  draw_button  incr_cols_button;
  draw_button  decr_cols_button;
  draw_button  save_button;
  draw_button  quit_button;
  synchronize ();
  loop ()
