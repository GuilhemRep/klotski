exception Invalid_move
exception Timeout
exception Terminate

let debug = false;;
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

(* For classic Klotski, OnlyX*)
let mode = Allpieces;;

(** Klotski (hard!) *)
let init_board:board = 
  [|
    [|'A';'X';'X';'C'|];  
    [|'A';'X';'X';'C'|];
    [|'B';'E';'E';'D'|];
    [|'B';'F';'G';'D'|];
    [|'H';'V';'V';'I'|];
  |];;

let end_board:board = 
  [|
    [|'A';'E';'E';'C'|];  
    [|'A';'F';'G';'C'|];
    [|'B';'V';'V';'D'|];
    [|'B';'X';'X';'D'|];
    [|'H';'X';'X';'I'|];
  |];;


(** Easy variant of Klotski *)
let init_board:board = 
[|
  [|'A';'A';'B';'B'|];  
  [|'A';'V';'V';'B'|];
  [|'D';'V';'V';'C'|];
  [|'D';'D';'C';'C'|];
  [|'V';'V';'V';'V'|];
  [|'V';'X';'X';'V'|];
  [|'V';'X';'X';'V'|];
|];;

let end_board:board = 
[|
  [|'A';'A';'B';'B'|];  
  [|'A';'X';'X';'B'|];
  [|'D';'X';'X';'C'|];
  [|'D';'D';'C';'C'|];
  [|'V';'V';'V';'V'|];
  [|'V';'V';'V';'V'|];
  [|'V';'V';'V';'V'|];
|];;

(** Test *)
let init_board:board = 
[|
  [|'A';'A';'B';'B'|];  
  [|'A';'V';'V';'B'|];
  [|'D';'V';'V';'C'|];
  [|'D';'D';'C';'C'|];
  [|'V';'X';'X';'V'|];
  [|'V';'X';'X';'V'|];
|];;

let end_board:board = 
[|
  [|'A';'A';'B';'B'|];  
  [|'A';'X';'X';'B'|];
  [|'D';'X';'X';'C'|];
  [|'D';'D';'C';'C'|];
  [|'V';'V';'V';'V'|];
  [|'V';'V';'V';'V'|];
|];;

(** Size of the game board *)
let size = (Array.length init_board,Array.length init_board.(0));;

(** An array with each type of piece only once *)
let pieces =
  let rec aux l p = match l with
    | []-> [p]
    | t::q when t==p -> l
    | _ when p=='V'  -> l
    | t::q -> t::(aux q p) in
  let l = ref [] in
  for x=0 to (fst size)-1 do 
    for y=0 to (snd size)-1 do 
      l := init_board.(x).(y) :: (!l);
    done;
  done;
  Array.of_list (List.fold_left aux [] (!l));;

(** Number of types of pieces *)
let number_pieces = Array.length pieces;;
  

(** Creates a fresh new board *)
let new_board () : board = 
  let a = Array.make (fst size) [||] in 
  for i=0 to (fst size)-1 do
    let b = Array.make (snd size) 'V' in 
    a.(i) <- b
  done;
  a

(** Two modes depending on the goal of the puzzle *)
let is_equal (b1:board) (b2:board) = 
  try (
    for x=0 to (fst size)-1 do
      for y=0 to (snd size)-1 do 
        match mode with
          | Allpieces -> if b1.(x).(y) <> b2.(x).(y) then raise Terminate
          | OnlyX     -> if (b1.(x).(y) == 'X' && b2.(x).(y) <> 'X') then raise Terminate
      done;
    done;
    true
  ) with Terminate -> false

(** copy [board] outputs a fresh board copied from [board]*)
let copy (board:board) : board = 
  let a = Array.make (fst size) [||] in 
  for x=0 to (fst size)-1 do
    let b = Array.make (snd size) 'V' in 
    for y=0 to (snd size)-1 do
      b.(y) <- board.(x).(y)
    done;
    a.(x) <- b
  done;
  a

(** Test if [copy] works *)
let () =
  if debug then (
    assert (is_equal end_board (copy end_board));
    assert (is_equal (copy init_board) (copy (copy init_board)))
  )

(* Test is copy is relevant *)
let () = 
  if debug && (mode == Allpieces) then (
    assert (
      let b1 = new_board() in 
      let b2 = copy b1 in 
      b2.(0).(0)<- 'A';
      not (is_equal b1 b2)
    );
    assert (
      let b1 = new_board() in 
      let b2 = b1 in 
      b2.(0).(0)<- 'A';
      is_equal b1 b2
    );
  )


let is_finished board =
  is_equal board end_board

(** Terminal format *)
let print_board (board:board) : unit =
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
  print_newline()

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
  let cell_size = 0.8 in (* Size of each cell in TikZ units *)

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
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5)
          ((float_of_int c +. 1.0) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5)
        )
        | Some (p,W) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=2mm]}] (%f,%f) -- (%f,%f);\n"
          ((float_of_int c +. 1.0) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5)
          (float_of_int c *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5)
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

let () =
  if false then ( 
  assert ( not (fst (apply ('C', S) init_board) ) );
  assert ( not (fst (apply ('D', E) init_board) ) );
  assert (     (fst (apply ('C', E) init_board) ) );
  assert (     (fst (apply ('D', W) init_board) ) )
  )


(** Checks the solution: l(b1)=?b2 *)
let rec check l b1 b2 = match l with
  | [] -> is_equal b1 b2
  | t::q -> (
    let (ok, b) = apply t b1 in
    if not ok then
      false
    else
      check q b b2
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

let bfs (start_board : board) (max_steps : int) : unit =
    let visited = Hashtbl.create max_steps in
    let queue = Queue.create () in
    Queue.push (start_board, 0, []) queue; (* (board, steps, solution) *)
    Hashtbl.add visited start_board ();
  
    while not (Queue.is_empty queue) do
      let (current_board, steps, solution) = Queue.pop queue in
  
      if debug then (
        Printf.printf "Step %d\n" steps;
        print_board current_board
      );
  
      if is_finished current_board then raise (Solution solution)
      else if steps < max_steps then (
        if debug then print_string "Detecting neighbours...\n";
  
        for d = 0 to number_directions - 1 do
          let current_direction = directions.(d) in
          for p = 0 to number_pieces - 1 do
            let current_piece = pieces.(p) in
            let (ok, next_board) = apply (current_piece, current_direction) current_board in
            if ok && not (Hashtbl.mem visited next_board) then (
              Hashtbl.add visited next_board ();
              Queue.push (next_board, steps + 1, (current_piece, current_direction) :: solution) queue;
            )
          done;
        done
      )
    done;
  
    if debug then print_string "No solution found\n";
    raise Timeout
    

let () =
  let max_steps = 1000 in
  let visited = Hashtbl.create max_steps in
  Hashtbl.add visited init_board ();
  try (bfs init_board max_steps)
  with Solution l -> (
    assert (check (List.rev l) init_board end_board));
    print_string (latex_solution (List.rev l) init_board )
 

