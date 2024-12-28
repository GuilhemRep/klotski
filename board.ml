exception Invalid_move
exception Timeout
exception Terminate

let debug = true;;
let unit_test = false;;

type piece = char

type direction = N | S | E | W
let directions = [|N;S;E;W|];;
let number_directions = Array.length directions;;

type board = piece array array
type move = piece * direction

exception Solution of move list

(* Allpieces if all the pieces in the solution must be at their place,
   OnlyX if only the position of the piece X matters.
   TODO: accept isomorphic positions *)
type mode = Allpieces | OnlyX 

(** An array with each type of piece only once *)
let pieces board =
  let size = (Array.length board,Array.length board.(0)) in
  let rec aux l p = match l with
    | []-> [p]
    | t::q when t==p -> l
    | _ when p=='V' || p=='O'  -> l
    | t::q -> t::(aux q p) in
  let l = ref [] in
  for x=0 to (fst size)-1 do 
    for y=0 to (snd size)-1 do 
      l := board.(x).(y) :: (!l);
    done;
  done;
  Array.of_list (List.fold_left aux [] (!l));;

(** Number of types of pieces *)
let number_pieces board = Array.length (pieces board);;

(** Creates a fresh new board *)
let new_board (size) : board = 
  let a = Array.make (fst size) [||] in 
  for i=0 to (fst size)-1 do
    let b = Array.make (snd size) 'V' in 
    a.(i) <- b
  done;
  a

(** Two modes depending on the goal of the puzzle *)
let is_equal (b1:board) (b2:board) mode = 
  let size1 = (Array.length b1,Array.length b1.(0)) in
  let size2 = (Array.length b2,Array.length b2.(0)) in
  if size1<>size2 then false
  else 
  try (
    for x=0 to (fst size1)-1 do
      for y=0 to (snd size1)-1 do 
        match mode with
          | Allpieces -> if b1.(x).(y) <> b2.(x).(y) then raise Terminate
          | OnlyX     -> if (b1.(x).(y) == 'X' && b2.(x).(y) <> 'X') then raise Terminate
      done;
    done;
    true
  ) with Terminate -> false

(** copy [board] outputs a fresh board copied from [board]*)
let copy (board:board) : board = 
  let size = (Array.length board,Array.length board.(0)) in
  let a = Array.make (fst size) [||] in 
  for x=0 to (fst size)-1 do
    let b = Array.make (snd size) 'V' in 
    for y=0 to (snd size)-1 do
      b.(y) <- board.(x).(y)
    done;
    a.(x) <- b
  done;
  a


let is_finished board end_board mode =
  is_equal board end_board mode

(** Terminal format *)
let print_board (board:board) : unit =
  let size = (Array.length board,Array.length board.(0)) in
  print_string " ";
  for i=0 to (snd size)-1 do
    print_string "_";
  done;
  print_string "\n";
  for i=0 to (fst size)-1 do
    print_string "|";
    for j=0 to (snd size)-1 do
      print_char (board.(i).(j))
    done;
    print_string "|\n";
  done;
  print_string " ";
  for i=0 to (snd size)-1 do
    print_string "-";
  done;
  print_newline();print_newline()

let move2string m = match m with
  | N -> "N"
  | S -> "S"
  | E -> "E"
  | W -> "W"

let print_move m = let (p,d) = m in 
  print_char p;
  print_string (move2string d)

let rec print_move_list l = match l with
  | []   -> ()
  | m::q -> (
    print_move m;
    print_string ",";
    print_move_list q
  )

let piece_to_color = function
  | 'A' -> "red"
  | 'B' -> "blue"
  | 'C' -> "green"
  | 'D' -> "orange"
  | 'E' -> "cyan"
  | 'F' -> "magenta"
  | 'G' -> "yellow"
  | 'H' -> "olive"
  | 'I' -> "teal"
  | 'J' -> "violet"
  | 'K' -> "lime"
  | 'X' -> "black"
  | _ -> "lightgray"


(* Function to generate TikZ code *)
let generate_tikz (board : board) (move:move Option.t) : string =
  let rows = Array.length board in
  let cols = Array.length board.(0) in
  let cell_size = 0.7 in (* Size of each cell in TikZ units *)

  (* Helper to generate TikZ code for a single cell *)
  let cell_to_tikz (r : int) (c : int) (piece : piece) : string =
    if piece = 'V' then "" (* Skip empty cells *)
    else
      let color = piece_to_color piece in
      Printf.sprintf
        "\\filldraw[%s] (%f, %f) rectangle (%f, %f);\n"
        color
        (float_of_int c *. cell_size)
        (float_of_int (rows - 1 - r) *. cell_size)
        ((float_of_int c +. 1.0) *. cell_size)
        ((float_of_int (rows - r)) *. cell_size);
      ^
      match move with
        | None -> ""
        | Some (p,N) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=2mm]}] (%f,%f) -- (%f,%f);\n"
          ((float_of_int c +. 0.5) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          ((float_of_int (rows - r)) *. cell_size)
        )
        | Some (p,S) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=2mm]}] (%f,%f) -- (%f,%f);\n"
          ((float_of_int c +. 0.5) *. cell_size)
          ((float_of_int (rows - r)) *. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size)
        )
        | Some (p,E) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=2mm]}] (%f,%f) -- (%f,%f);\n"
          (float_of_int c *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
          ((float_of_int c +. 1.0) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
        )
        | Some (p,W) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=2mm]}] (%f,%f) -- (%f,%f);\n"
          ((float_of_int c +. 1.0) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
          (float_of_int c *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
        )
        | _ -> "";
  in

  (* Generate TikZ rectangles for all cells *)
  let tikz_rectangles =
    Array.mapi (fun r row ->
        Array.mapi (fun c piece -> cell_to_tikz r c piece) row |> Array.to_list)
      board
    |> Array.to_list
    |> List.flatten
    |> String.concat ""
  in

  (* Add a black border around the entire board *)
  let border =
    Printf.sprintf
      "\\draw[black, thick] (0, 0) rectangle (%f, %f);\n"
      (float_of_int cols *. cell_size)
      (float_of_int rows *. cell_size)
  in

  (* \draw [-{Latex[length=5mm]}] (0,0) -- (2,0); *)

  (* Wrap the TikZ code in a figure *)
  Printf.sprintf
    "\\begin{tikzpicture}[x=%fcm, y=%fcm]\n%s%s\\end{tikzpicture}\n"
    cell_size
    cell_size
    tikz_rectangles
    border

(** [apply move board] returns a couple [(v,b)] such that [v] is true iff the move [move] can be applied to [board],
  and [b] is the resulting board. If [v] is false, then [b] is irrelevant *)
let apply (move : move) (board:board) : bool*board =
  let size = (Array.length board,Array.length board.(0)) in
  let new_board = copy board in
  let p,d = move in
  try (
    (match d with 
      | N -> (
        for x=0 to (fst size)-1 do
          for y=0 to (snd size)-1 do
            if board.(x).(y) == p then (
        if x==0 then raise Invalid_move
        else if not (board.(x-1).(y) == p || board.(x-1).(y) == 'V') then raise Invalid_move;
        new_board.(x-1).(y) <- p;
        new_board.(x).(y) <- 'V'
            )
      done;
    done;
    )
    
    | S -> (
      for x=(fst size)-1 downto 0 do
        for y=(snd size)-1 downto 0 do
          if board.(x).(y) == p then (
        if x==(fst size)-1 then raise Invalid_move
        else if not (board.(x+1).(y) == p || board.(x+1).(y) == 'V') then raise Invalid_move;
        new_board.(x+1).(y) <- p; 
        new_board.(x).(y) <- 'V'
          )
      done;
    done;
    )

    | E -> (
      for x=(fst size)-1 downto 0 do
        for y=(snd size)-1 downto 0 do
          if board.(x).(y) == p then (
        if y==(snd size)-1 then raise Invalid_move
        else if not (board.(x).(y+1) == p || board.(x).(y+1) == 'V') then raise Invalid_move;
        new_board.(x).(y+1) <- p;
        new_board.(x).(y) <- 'V'
          )
      done;
    done;
    )

    | W -> (
      for x=0 to (fst size)-1 do
        for y=0 to (snd size)-1 do
          if board.(x).(y) == p then (
        if y==0 then raise Invalid_move
        else if not (board.(x).(y-1) == p || board.(x).(y-1) == 'V') then raise Invalid_move;
        new_board.(x).(y-1) <- p;
        new_board.(x).(y) <- 'V'
          )
      done;
    done;
  ));
  (true,new_board)

  ) with Invalid_move -> (false, new_board)


(** Checks the solution: l(b1)=?b2 *)
let rec check l b1 b2 mode = match l with
  | [] -> is_equal b1 b2 mode
  | t::q -> (
    let (ok, b) = apply t b1 in
    if not ok then
      false
    else
      check q b b2 mode
  )

let rec latex_solution l b1 = match l with
  | [] -> (
    let tikz_code = generate_tikz b1 None in
    tikz_code
  )
  | t::q -> (
    let (ok, b) = apply t b1 in
    let tikz_code = generate_tikz b1 (Some t) in
    tikz_code^(latex_solution q b)
  )

let opposite m = match m with
  | N -> S
  | E -> W
  | W -> E
  | S -> N

let bfs (start_board : board) (end_board : board) (mode: mode) (max_steps : int) : unit =
  let discovered = Hashtbl.create max_steps in
  let queue = Queue.create () in
  let rec generate_solution board =
    assert (Hashtbl.mem discovered board);
    let optm = Hashtbl.find discovered board in
    match optm with
    | None -> []
    | Some (p,d) -> (
      let (ok, new_board) = apply (p, opposite d) board in
      assert ok;
      (p,d)::(generate_solution new_board)
    )
  in
  let steps = ref 0 in
  Queue.push start_board queue;
  Hashtbl.add discovered start_board None;

  while not (Queue.is_empty queue) do
    let current_board = Queue.pop queue in

    if debug then (
      Printf.printf "Step %d\n" !steps;
      print_board current_board
    );

    if is_finished current_board end_board mode then raise (Solution (List.rev (generate_solution current_board)))
    else if !steps < max_steps then (
      incr steps;
      if debug then print_string "Detecting neighbours...\n";

      for d = 0 to number_directions - 1 do
        let current_direction = directions.(d) in
        for p = 0 to (number_pieces start_board) - 1 do
          let current_piece = (pieces start_board).(p) in
          let (ok, next_board) = apply (current_piece, current_direction) current_board in
          if ok && not (Hashtbl.mem discovered next_board) then (
            Hashtbl.add discovered next_board (Some (current_piece, current_direction));
            Queue.push next_board queue;
          )
        done;
      done
    )
  done;
  failwith "No solution"

let bfs_double (start_board : board) (end_board : board) (mode: mode) (max_steps : int) : unit =
  let discovered = Hashtbl.create max_steps in
  let visited = Hashtbl.create max_steps in
  let queue1 = Queue.create () in
  let queue2 = Queue.create () in
  let rec generate_solution board =
    assert (Hashtbl.mem discovered board);
    let optm = Hashtbl.find discovered board in
    match optm with
    | None -> []
    | Some (p,d) -> (
      let (ok, new_board) = apply (p, opposite d) board in
      assert ok;
      (p,d)::(generate_solution new_board)
    )
  in
  let steps = ref 0 in
  Queue.push start_board queue1;
  Queue.push end_board queue2;
  Hashtbl.add discovered start_board None;
  Hashtbl.add discovered end_board None;

  while not (Queue.is_empty queue1 || Queue.is_empty queue2) do
    let current_board1 = Queue.pop queue1 in
    if debug then (
      Printf.printf "Step %d\n" !steps;
      print_board current_board1
    );

    if Hashtbl.mem visited current_board1 then raise (Solution (List.rev (generate_solution current_board1)))
    else if !steps < max_steps then (
      Hashtbl.add visited current_board1 ();
      incr steps;
      if debug then print_string "--->Detecting neighbours...\n";

      for d = 0 to number_directions - 1 do
        let current_direction = directions.(d) in
        for p = 0 to (number_pieces start_board) - 1 do
          let current_piece = (pieces start_board).(p) in
          let (ok, next_board) = apply (current_piece, current_direction) current_board1 in
          if ok && not (Hashtbl.mem discovered next_board) then (
            Hashtbl.add discovered next_board (Some (current_piece, current_direction));
            Queue.push next_board queue1;
          )
        done;
      done
    );


    let current_board2 = Queue.pop queue2 in
    if debug then (
      Printf.printf "Step %d\n" !steps;
      print_board current_board2
    );

    if Hashtbl.mem visited current_board2 then raise (Solution (List.rev (generate_solution current_board2)))
    else if !steps < max_steps then (
      Hashtbl.add visited current_board2 ();
      incr steps;
      if debug then print_string "<---Detecting neighbours...\n";

      for d = 0 to number_directions - 1 do
        let current_direction = directions.(d) in
        for p = 0 to (number_pieces start_board) - 1 do
          let current_piece = (pieces start_board).(p) in
          let (ok, next_board) = apply (current_piece, current_direction) current_board2 in
          if ok && not (Hashtbl.mem discovered next_board) then (
            Hashtbl.add discovered next_board (Some (current_piece, current_direction));
            Queue.push next_board queue2;
          )
        done;
      done
    )
  done;
  failwith "No solution"

let write_file (file:string) (s:string) =
  let oc = open_out file in
  Printf.fprintf oc "%s" s

let string_to_board (s : string) : board =
  (* let rec remove_last l = match l with
    | [] -> failwith "Bizar"
    | t::[] -> []
    |t::q -> t::(remove_last q) in *)
  let l = String.split_on_char '\n' s in
  (* let lines = remove_last l in *)
  let lines = l in
  Array.of_list (List.map (fun line -> Array.of_list (List.init (String.length line) (String.get line))) lines)
(*   

let () =
  if (Array.length Sys.argv) = 4 then (
    let max_steps = 100000 in
    try (bfs (string_to_board (Sys.argv.(1))) max_steps)
    with Solution l -> (
      assert (check (List.rev l) init_board end_board));
      print_string (latex_solution (List.rev l) init_board )
  ) else (
    print_string "Not enough arguments"
  ) *)
