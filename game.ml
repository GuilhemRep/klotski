module Game :
  sig
    type board
    type move
    type mode = Allpieces | OnlyX | Shape
    exception Solution of move list
    exception NoSolution
    val solve : board -> board -> mode -> int -> unit
    val string_to_board : string -> board
    val print_board : board -> unit
    val latex_solution : move list -> board -> string
    val write_file : string -> string -> unit 
    val simple_latex : move list -> board -> string
  end 
= struct 
  let debug = false;;

  type piece = char

  type direction = N | S | E | W
  let directions = [|N;S;E;W|];;
  let number_directions = Array.length directions;;

  type board = piece array array
  type move = piece * direction

  exception Invalid_move
  exception Timeout
  exception Terminate
  exception NoSolution
  exception Solution of move list


  (** Allpieces if all the pieces in the solution must be at their place,
    OnlyX if only the position of the piece X matters.
    Shape if only the global chape matters *)
  type mode = Allpieces | OnlyX | Shape

  (** An array with each type of piece only once, in the order of appearance *)
  let pieces board =
    let size = (Array.length board,Array.length board.(0)) in
    let rec aux_fun l p = match l with
        _ when (p=='_' || p=='.')  -> l
      | []-> [p]
      | t::q when t==p -> l
      | t::q -> t::(aux_fun q p) in
    let l = ref [] in
    for x=0 to (fst size)-1 do 
      for y=0 to (snd size)-1 do 
        l := board.(x).(y) :: (!l);
      done;
    done;
    l := List.rev (!l);
    let a = (Array.of_list (List.fold_left aux_fun [] (!l))) in
    (* Array.fast_sort compare a; *)
    a
  
  let () =
    assert (pieces [|[|'A';'B';'C'|];[|'A';'B';'C'|];[|'A';'B';'C'|]|] = [|'A';'B';'C'|]);
    assert (pieces [|[|'C';'C';'C'|];[|'A';'B';'C'|];[|'A';'B';'C'|]|] = [|'C';'A';'B'|]);
    assert (pieces [|[|'C';'B';'C'|];[|'X';'B';'C'|];[|'A';'B';'C'|]|] = [|'C';'B';'X';'A'|])

  (** Number of types of pieces *)
  let number_pieces board = Array.length (pieces board)

  (** check obvious absence of solution*)
  (*TODO: better version*)
  let basic_check b1 b2 =
    let size1 = (Array.length b1,Array.length b1.(0)) in
    let size2 = (Array.length b2,Array.length b2.(0)) in
    if size1<>size2 then raise NoSolution;
    if Array.fast_sort compare (pieces b1) <> Array.fast_sort compare (pieces b2)
      then raise NoSolution

  (** Creates a fresh new board *)
  let new_board (size) : board = 
    let a = Array.make (fst size) [||] in 
    for i=0 to (fst size)-1 do
      let b = Array.make (snd size) '_' in 
      a.(i) <- b
    done;
    a

  (** Modes depending on the goal of the puzzle *)
  let is_equal (b1:board) (b2:board) mode = 
    let size = (Array.length b1,Array.length b1.(0)) in
    try (
      for x=0 to (fst size)-1 do
        for y=0 to (snd size)-1 do 
          match mode with
            | Allpieces -> if b1.(x).(y) <> b2.(x).(y) then raise Terminate
            | OnlyX     -> if (b1.(x).(y) == 'X' && b2.(x).(y) <> 'X') then raise Terminate
            | Shape     -> if  (b1.(x).(y) == '_' && b2.(x).(y) <> '_')
                            || (b2.(x).(y) == '_' && b1.(x).(y) <> '_') then raise Terminate 
        done;
      done;
      true
    ) with Terminate -> false

  (** copy [board] outputs a fresh board copied from [board]*)
  let copy (board:board) : board = 
    let size = (Array.length board,Array.length board.(0)) in
    let a = Array.make (fst size) [||] in 
    for x=0 to (fst size)-1 do
      let b = Array.make (snd size) '_' in 
      for y=0 to (snd size)-1 do
        b.(y) <- board.(x).(y)
      done;
      a.(x) <- b
    done;
    a

  (** Tests whether the puzzle is solved *)
  let is_finished board end_board mode =
    is_equal board end_board mode

  (** Renames the pieces of a board [b] such that the result would be the same for
  any board obtained by renaming pieces of [b]  *)
  let canonical (b : board) : board = 
    let copy_b =  copy b in 
    let pieces_b = pieces b in 
    let dict = Hashtbl.create (number_pieces b) in 
    let count_piece = ref 0 in
    for i=0 to number_pieces b - 1 do 
      let current_piece = pieces_b.(i) in 
      if current_piece = 'X' then Hashtbl.add dict 'X' 'X'
      else (
        Hashtbl.add dict current_piece (Char.chr (65 + !count_piece)) ; 
        incr count_piece
        )
    done;
 
    let size = (Array.length b,Array.length b.(0)) in 
    for i = 0 to (fst size - 1) do 
      for j = 0 to (snd size - 1) do 
        let y = Hashtbl.find_opt dict (b.(i).(j)) in match y with
        | None -> (
          if b.(i).(j) = '_' then copy_b.(i).(j) <- '_'
          else if b.(i).(j) = '.' then copy_b.(i).(j) <- '.'
          else failwith "Unknown character in board"
        )
        | Some a -> copy_b.(i).(j) <- a
      done;
    done;
    copy_b

  let () =
      assert (canonical [|[|'A' ; 'B' |] ; [|'C' ; 'Z'|]|] = [|[|'A' ; 'B' |] ; [|'C' ; 'D'|]|]) ;
      assert (canonical [|[|'C' ; 'C' |] ; [|'B' ; 'D'|]|] = [|[|'A' ; 'A' |] ; [|'B' ; 'C'|]|]) ;
      assert (canonical [|[|'C' ; '_' |] ; [|'B' ; 'X'|]|] = [|[|'A' ; '_' |] ; [|'B' ; 'X'|]|]) ;
      assert (canonical [|[|'C' ; '_' |] ; [|'B' ; 'X'|]|] = canonical ( canonical[|[|'C' ; '_' |] ; [|'B' ; 'X'|]|])) ;
      assert (canonical [|[|'A';'B';'C'|];[|'A';'B';'C'|];[|'A';'B';'W'|]|]
      = [|[|'A';'B';'C'|];[|'A';'B';'C'|];[|'A';'B';'D'|]|]) ;
      assert (canonical [|[|'D';'B';'C'|];[|'A';'B';'C'|];[|'A';'B';'W'|]|]
      = [|[|'A';'B';'C'|];[|'D';'B';'C'|];[|'D';'B';'E'|]|]) ;
      assert (canonical [|[|'C';'B';'A'|];[|'C';'B';'A'|];[|'C';'B';'A'|]|]
      = [|[|'A';'B';'C'|];[|'A';'B';'C'|];[|'A';'B';'C'|]|])

  (** Terminal readable format *)
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
    | 'X' -> "darkgray"
    | _ -> "white"

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
          else if not (board.(x-1).(y) == p || board.(x-1).(y) == '_') then raise Invalid_move;
          new_board.(x-1).(y) <- p;
          new_board.(x).(y) <- '_'
              )
        done;
      done;
      )
      
      | S -> (
        for x=(fst size)-1 downto 0 do
          for y=(snd size)-1 downto 0 do
            if board.(x).(y) == p then (
          if x==(fst size)-1 then raise Invalid_move
          else if not (board.(x+1).(y) == p || board.(x+1).(y) == '_') then raise Invalid_move;
          new_board.(x+1).(y) <- p; 
          new_board.(x).(y) <- '_'
            )
        done;
      done;
      )

      | E -> (
        for x=(fst size)-1 downto 0 do
          for y=(snd size)-1 downto 0 do
            if board.(x).(y) == p then (
          if y==(snd size)-1 then raise Invalid_move
          else if not (board.(x).(y+1) == p || board.(x).(y+1) == '_') then raise Invalid_move;
          new_board.(x).(y+1) <- p;
          new_board.(x).(y) <- '_'
            )
        done;
      done;
      )

      | W -> (
        for x=0 to (fst size)-1 do
          for y=0 to (snd size)-1 do
            if board.(x).(y) == p then (
          if y==0 then raise Invalid_move
          else if not (board.(x).(y-1) == p || board.(x).(y-1) == '_') then raise Invalid_move;
          new_board.(x).(y-1) <- p;
          new_board.(x).(y) <- '_'
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
  
  (** North is opposite of south and so on *)
  let opposite m = match m with
    | N -> S
    | E -> W
    | W -> E
    | S -> N

  (** Vertical symetry of the board *)
  let symetrical (board : board) : board = 
    let size = (Array.length board,Array.length board.(0)) in
    let a = Array.make (fst size) [||] in 
    for x=0 to (fst size)-1 do
      let b = Array.make (snd size) '_' in 
      for y=0 to (snd size)-1 do
        b.(y) <- board.(x).((snd size)-1 - y)
      done;
      a.(x) <- b
    done;
    a

  let solve (start_board : board) (end_board : board) (mode: mode) (max_steps : int) : unit =
    basic_check start_board end_board;
    let discovered = Hashtbl.create max_steps in
    let deja_vu = Hashtbl.create max_steps in
    let queue = Queue.create () in
    let rec unfold_solution board =
      assert (Hashtbl.mem discovered board);
      let optm = Hashtbl.find discovered board in
      match optm with
      | None -> []
      | Some (p,d) -> (
        let (ok, new_board) = apply (p, opposite d) board in
        assert ok;
        (p,d)::(unfold_solution new_board)
      )
    in
    let steps = ref 0 in
    let number_pieces = number_pieces start_board in
    let the_pieces = pieces start_board in 
    
    Queue.push start_board queue;
    Hashtbl.add discovered start_board None;
    Hashtbl.add deja_vu (canonical start_board) ();

    if is_finished start_board end_board mode then (
      let l = unfold_solution start_board in
      raise (Solution (List.rev l))
    );

    while not (Queue.is_empty queue) do
      let current_board = Queue.pop queue in

      if debug then (
        Printf.printf "Step %d\n" !steps;
        print_board current_board
      );

      if !steps < max_steps then (
        incr steps;
        if debug then print_string "Finding neighbours... ";
        let number_neighbours = ref 0 in
        for p = 0 to number_pieces - 1 do
          let current_piece = the_pieces.(p) in
            for d = 0 to number_directions - 1 do
              let current_direction = directions.(d) in
              let (ok, next_candidate) = apply (current_piece, current_direction) current_board in
              if ok then 
                if Hashtbl.mem deja_vu (canonical next_candidate) || Hashtbl.mem deja_vu (canonical (symetrical next_candidate))
                  then ()
                else (
                  Hashtbl.add discovered next_candidate (Some (current_piece, current_direction));
                  if is_finished next_candidate end_board mode then (
                    let l = unfold_solution next_candidate in
                    raise (Solution (List.rev l))
                  ) else (
                    Hashtbl.add deja_vu (canonical next_candidate) ();
                    Queue.push next_candidate queue;
                    incr number_neighbours
                  )
                )
          done;
        done;
        if debug then print_int (!number_neighbours);
        if debug then print_string "\n";
      )
      else failwith "No solution found..."
    done;
    failwith "No solution"


(* ------- LaTeX part ------- *)

  let cell_size () = 0.6

  (* Function to generate TikZ code *)
  let generate_tikz (board : board) (move:move Option.t) : string =
    let rows = Array.length board in
    let cols = Array.length board.(0) in
    let cell_size = cell_size() in (* Size of each cell in TikZ units *)

    (* Helper to generate TikZ code for a single cell *)
    let cell_to_tikz (r : int) (c : int) (piece : piece) : string =
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
          "\\draw [white, -{Latex[length=%fmm]}] (%f,%f) -- (%f,%f);\n"
          (4.*. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          ((float_of_int (rows - r)) *. cell_size)
        )
        | Some (p,S) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=%fmm]}] (%f,%f) -- (%f,%f);\n"
          (4.*. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          ((float_of_int (rows - r)) *. cell_size)
          ((float_of_int c +. 0.5) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size)
        )
        | Some (p,E) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=%fmm]}] (%f,%f) -- (%f,%f);\n"
          (4.*. cell_size)
          (float_of_int c *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
          ((float_of_int c +. 1.0) *. cell_size)
          (float_of_int (rows - 1 - r) *. cell_size +. 0.5 *. cell_size)
        )
        | Some (p,W) when piece==p -> (
          Printf.sprintf
          "\\draw [white, -{Latex[length=%fmm]}] (%f,%f) -- (%f,%f);\n"
          (4.*. cell_size)
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

  let latex_solution l b1 = 
    let rec aux l b1 m c= match l with
    | [] -> (
      let tikz_code = generate_tikz b1 None in
      tikz_code
    )
    | t::q -> (
      let (ok, b) = apply t b1 in
      let tikz_code = generate_tikz b1 (Some t) in
      tikz_code^(if c mod m == 0 then "\n" else "")^(aux q b m (c+1))
    ) in
    let cols = Array.length b1.(0) in
    let cell_size = cell_size () in 
    let size_figure = (Int.to_float cols) *. cell_size in
    let page_width = 150. in
    let number_per_line = Float.to_int (page_width /. size_figure) in
    aux l b1 number_per_line 1

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
      | [] -> invalid_arg "empty_string"
      | t::[] -> []
      |t::q -> t::(remove_last q) in
    let l = String.split_on_char '\n' s in
    let lines = remove_last l in
    let board = Array.of_list (List.map (fun line -> Array.of_list (List.init (String.length line) (String.get line))) lines) in
    for i=0 to (Array.length board -1) do
      for j=0 to (Array.length board.(0) -1) do
        if board.(i).(j) == '_' then board.(i).(j) <- '_'
      done;
    done;
    board

end