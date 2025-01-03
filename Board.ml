module Board : sig
  type board
  type move
  type mode = Allpieces | OnlyX 
  exception Solution of move list
  exception NoSolution
  val solve : board -> board -> mode -> int -> unit
  val string_to_board : string -> board
  val print_board : board -> unit
  val latex_solution : move list -> board -> string
  val write_file : string -> string -> unit 
  val simple_latex : move list -> board -> string
end = struct 
  exception Invalid_move
  exception Timeout
  exception Terminate
  exception NoSolution

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

  (** An array with each type of piece only once *)
  let pieces board =
    let size = (Array.length board,Array.length board.(0)) in
    let rec aux l p = match l with
      | _ when (p==' ' || p=='.')  -> l
      | []-> [p]
      | t::q when t==p -> l
      | t::q -> t::(aux q p) in
    let l = ref [] in
    for x=0 to (fst size)-1 do 
      for y=0 to (snd size)-1 do 
        l := board.(x).(y) :: (!l);
      done;
    done;
    let a = (Array.of_list (List.fold_left aux [] (!l))) in
    Array.fast_sort compare a;
    a

  (** Number of types of pieces *)
  let number_pieces board = Array.length (pieces board)

  (** verifys for obvious absence of solution*)
  (*TODO: better version*)
  let basic_check b1 b2 =
    if pieces b1 <> pieces b2 then raise NoSolution

  (** Creates a fresh new board *)
  let new_board (size) : board = 
    let a = Array.make (fst size) [||] in 
    for i=0 to (fst size)-1 do
      let b = Array.make (snd size) ' ' in 
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
      let b = Array.make (snd size) ' ' in 
      for y=0 to (snd size)-1 do
        b.(y) <- board.(x).(y)
      done;
      a.(x) <- b
    done;
    a

  (** Tests whether the puzzle is solved *)
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

    (* 
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
    ) *)

  (** For the latex output *)
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
    | 'L' -> "pink"
    | 'M' -> "teal"
    | 'N' -> "purple"
    | 'P' -> "brown"
    | '.' -> "lightgray"
    | 'X' -> "black"
    | _ -> "gray"


  (* Function to generate TikZ code *)
  let generate_tikz (board : board) (move:move Option.t) : string =
    let rows = Array.length board in
    let cols = Array.length board.(0) in
    let cell_size = 0.7 in (* Size of each cell in TikZ units *)

    (* Helper to generate TikZ code for a single cell *)
    let cell_to_tikz (r : int) (c : int) (piece : piece) : string =
      if piece = ' ' then "" (* Skip empty cells *)
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
        (* Arrows *)
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

     (* Helper to draw black borders between different pieces *)
     let draw_borders () =
      let borders = ref "" in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          let piece = board.(r).(c) in
          (* Compare with the cell to the right (vertical line) *)
          if c < cols - 1 && piece <> board.(r).(c + 1) then
            borders := !borders ^
                       Printf.sprintf
                         "\\draw[black, thick] (%f, %f) -- (%f, %f);\n"
                         ((float_of_int (c + 1)) *. cell_size)
                         ((float_of_int (rows - 1 - r)) *. cell_size)
                         ((float_of_int (c + 1)) *. cell_size)
                         ((float_of_int (rows - r)) *. cell_size);
          (* Compare with the cell below (horizontal line) *)
          if r < rows - 1 && piece <> board.(r + 1).(c) then
            borders := !borders ^
                       Printf.sprintf
                         "\\draw[black, thick] (%f, %f) -- (%f, %f);\n"
                         ((float_of_int c) *. cell_size)
                         ((float_of_int (rows - r-1)) *. cell_size)
                         ((float_of_int (c + 1)) *. cell_size)
                         ((float_of_int (rows - r-1)) *. cell_size)
        done
      done;
      !borders
    in
    (* Add board borders between pieces *)
    let borders = draw_borders () in

    (* Wrap the TikZ code in a figure *)
    Printf.sprintf
      "\\begin{tikzpicture}[x=%fcm, y=%fcm]\n%s%s%s\\end{tikzpicture}\n"
      cell_size
      cell_size
      tikz_rectangles
      border
      borders

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
          else if not (board.(x-1).(y) == p || board.(x-1).(y) == ' ') then raise Invalid_move;
          new_board.(x-1).(y) <- p;
          new_board.(x).(y) <- ' '
              )
        done;
      done;
      )
      
      | S -> (
        for x=(fst size)-1 downto 0 do
          for y=(snd size)-1 downto 0 do
            if board.(x).(y) == p then (
          if x==(fst size)-1 then raise Invalid_move
          else if not (board.(x+1).(y) == p || board.(x+1).(y) == ' ') then raise Invalid_move;
          new_board.(x+1).(y) <- p; 
          new_board.(x).(y) <- ' '
            )
        done;
      done;
      )

      | E -> (
        for x=(fst size)-1 downto 0 do
          for y=(snd size)-1 downto 0 do
            if board.(x).(y) == p then (
          if y==(snd size)-1 then raise Invalid_move
          else if not (board.(x).(y+1) == p || board.(x).(y+1) == ' ') then raise Invalid_move;
          new_board.(x).(y+1) <- p;
          new_board.(x).(y) <- ' '
            )
        done;
      done;
      )

      | W -> (
        for x=0 to (fst size)-1 do
          for y=0 to (snd size)-1 do
            if board.(x).(y) == p then (
          if y==0 then raise Invalid_move
          else if not (board.(x).(y-1) == p || board.(x).(y-1) == ' ') then raise Invalid_move;
          new_board.(x).(y-1) <- p;
          new_board.(x).(y) <- ' '
            )
        done;
      done;
    ));
    (true,new_board)

    ) with Invalid_move -> (false, new_board)

  (** verifies the solution: l(b1)=?b2 *)
  let rec verify l b1 b2 mode = match l with
    | [] -> is_equal b1 b2 mode
    | t::q -> (
      let (ok, b) = apply t b1 in
      if not ok then
        false
      else
        verify q b b2 mode
    )

  let latex_solution l b1 = 
    let rec aux l b1 = match l with
    | [] -> (
      let tikz_code = generate_tikz b1 None in
      tikz_code
    )
    | t::q -> (
      let (ok, b) = apply t b1 in
      let tikz_code = generate_tikz b1 (Some t) in
      tikz_code^(aux q b)
    ) in
    
    aux l b1

  let opposite m = match m with
    | N -> S
    | E -> W
    | W -> E
    | S -> N

  let solve (start_board : board) (end_board : board) (mode: mode) (max_steps : int) : unit =
    (* basic_check start_board end_board; *)
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

      if is_finished current_board end_board mode then (
        let l = (List.rev (generate_solution current_board)) in
        if debug then print_string (latex_solution l start_board);
        raise (Solution l)
      )
      else if !steps < max_steps then (
        incr steps;
        if debug then print_string "Detecting neighbours...\n";

        for p = 0 to (number_pieces start_board) - 1 do
          let current_piece = (pieces start_board).(p) in
            for d = 0 to number_directions - 1 do
              let current_direction = directions.(d) in
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

  let solve_double (start_board : board) (end_board : board) (mode: mode) (max_steps : int) : unit =
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

  let simple_latex l start_board = 
("\\documentclass[12pt]{article}
\\usepackage{multicol}
\\setlength{\\parindent}{0cm}
\\usepackage[a4paper,top=1cm,bottom=1cm,left=1cm,right=1cm]{geometry}
\\usepackage{amsmath,tikz-cd}
\\begin{document}
\\begin{center}
\\end{center}"^(latex_solution l start_board)^"\\end{document}")

  let write_file (file:string) (s:string) =
    let oc = open_out file in
    Printf.fprintf oc "%s" s

  let string_to_board (s : string) : board =
    let rec remove_last l = match l with
      | [] -> failwith "Bizar"
      | t::[] -> []
      |t::q -> t::(remove_last q) in
    let l = String.split_on_char '\n' s in
    let lines = remove_last l in
    (* let lines = l in *)
    Array.of_list (List.map (fun line -> Array.of_list (List.init (String.length line) (String.get line))) lines)
end