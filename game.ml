module Game :
  sig
    type board
    type move
    type mode = Allpieces | OnlyX | Shape
    exception Solution of move list
    exception NoSolution
    exception Value of int
    val solve : board -> board -> mode -> int -> unit
    val string_to_board : string -> board
    val print_board : board -> unit
    val latex_solution : move list -> board -> string
    val write_file : string -> string -> unit 
    val simple_latex : move list -> board -> string
  end 
= struct 
  exception Invalid_move
  exception Timeout
  exception Terminate
  exception NoSolution
  exception Value of int

  let debug = true;;

  type piece = char

  type direction = N | S | E | W
  let directions = [|N;S;E;W|];;
  let number_directions = Array.length directions;;

  type board = piece array array
  type move = piece * direction

  exception Solution of move list

  (** Allpieces if all the pieces in the solution must be at their place,
    OnlyX if only the position of the piece X matters.
    Shape if only the global chape matters *)
  type mode = Allpieces | OnlyX | Shape

  (** An array with each type of piece only once, in the order of appearance *)
  let pieces board =
    let size = (Array.length board,Array.length board.(0)) in
    let rec aux l p = match l with
      | _ when (p==' ' || p=='_' || p=='.')  -> l
      | []-> [p]
      | t::q when t==p -> l
      | t::q -> t::(aux q p) in
    let l = ref [] in
    for x=0 to (fst size)-1 do 
      for y=0 to (snd size)-1 do 
        l := board.(x).(y) :: (!l);
      done;
    done;
    l := List.rev (!l);
    let a = (Array.of_list (List.fold_left aux [] (!l))) in
    (* Array.fast_sort compare a; *)
    a

  (** Number of types of pieces *)
  let number_pieces board = Array.length (pieces board)

  (** check obvious absence of solution*)
  (*TODO: better version*)
  let basic_check b1 b2 =
    if Array.fast_sort compare (pieces b1) <> Array.fast_sort compare (pieces b2)
      then raise NoSolution

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
            | Shape     -> if  (b1.(x).(y) == ' ' && b2.(x).(y) <> ' ')
                            || (b1.(x).(y) == '_' && b2.(x).(y) <> '_')
                            || (b2.(x).(y) == ' ' && b1.(x).(y) <> ' ')
                            || (b2.(x).(y) == '_' && b1.(x).(y) <> '_') then raise Terminate 
        done;
      done;
      true
    ) with Terminate -> false

  let is_similar (b1 : board) (b2 : board) : bool = 
    let size1 = (Array.length b1,Array.length b1.(0)) in
    let size2 = (Array.length b2,Array.length b2.(0)) in
    if size1<>size2 then false
    else
    let aux b1 b2 = 
      let dict = Hashtbl.create (number_pieces b1) in
      for i = 0 to (fst size1 - 1) do 
        for j = 0 to (snd size1 - 1) do 
          match Hashtbl.find_opt dict (b1.(i).(j)) with
          | Some x -> if x<>(b2.(i).(j)) then raise Terminate else ()
          | None -> Hashtbl.add dict (b1.(i).(j)) (b2.(i).(j))
        done;
      done in 
      try (
        aux b1 b2 ;
        aux b2 b1 ;
        true
      ) with _ -> false

  let () =
    assert (is_similar [|[|'A' ; 'B' ; 'A'|] ; [|'C' ; 'B' ; 'A'|]|] [|[|'A' ; 'B' ; 'A'|] ; [|'C' ; 'B' ; 'A'|]|]);
    assert (is_similar [|[|'A' ; 'B' ; 'A'|] ; [|'C' ; 'B' ; 'A'|]|] [|[|'Z' ; 'B' ; 'Z'|] ; [|'C' ; 'B' ; 'Z'|]|])

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

  let canonical (b : board) : board = 
    let b_ =  copy b in 
    let p = pieces b in 
    let dict = Hashtbl.create (4+number_pieces b) in 
    Array.iteri (fun i x -> match x with
      |'X' -> Hashtbl.add dict x x 
      | _  -> Hashtbl.add dict (p.(i)) (Char.chr (65+i)) ) p ;
    let size = (Array.length b,Array.length b.(0)) in 
    for i = 0 to (fst size - 1) do 
        for j = 0 to (snd size - 1) do 
          let y = Hashtbl.find_opt dict (b.(i).(j)) in match y with
          | None -> b_.(i).(j) <- '_' 
          | Some a -> b_.(i).(j) <- a
        done;
      done;
    b_

  let () =
      assert (canonical [|[|'A' ; 'B' |] ; [|'C' ; 'Z'|]|] = [|[|'A' ; 'B' |] ; [|'C' ; 'D'|]|]) ;
      assert (canonical [|[|'C' ; 'C' |] ; [|'B' ; 'D'|]|] = [|[|'A' ; 'A' |] ; [|'B' ; 'C'|]|]) ;
      assert (canonical [|[|'C' ; ' ' |] ; [|'B' ; 'X'|]|] = [|[|'A' ; '_' |] ; [|'B' ; 'X'|]|]) ;
      assert (canonical [|[|'C' ; ' ' |] ; [|'B' ; 'X'|]|] = canonical ( canonical[|[|'C' ; '_' |] ; [|'B' ; 'X'|]|]))

  let () =
    let b = [|[|'C' ; 'C' |] ; [|'B' ; 'D'|]|] in 
    assert (is_similar b (canonical b))

  let canonical b = b

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

  

  let opposite m = match m with
    | N -> S
    | E -> W
    | W -> E
    | S -> N

  let symetrical (board : board) : board = 
    let size = (Array.length board,Array.length board.(0)) in
    let a = Array.make (fst size) [||] in 
    for x=0 to (fst size)-1 do
      let b = Array.make (snd size) ' ' in 
      for y=0 to (snd size)-1 do
        b.(y) <- board.(x).((snd size)-1 - y)
      done;
      a.(x) <- b
    done;
    a

  (** TODO (doesn't work) *)
  let is_symetrical (b : board) : bool = b = symetrical b
    
  let () = 
    assert(not(is_symetrical [|[|'A' ; 'B'|]|]));
    assert(    is_symetrical [|[|'A' ; 'A'|]|]);
    assert(not(is_symetrical [|[|'A' ; 'B' ; 'A'|] ; [|'C' ; 'B' ; 'A'|]|]));
    assert(not(is_symetrical [|[|'A' ; 'B' ; 'C'|] ; [|'A' ; 'B' ; 'C'|]|]));
    assert(    is_symetrical [|[|'A' ; 'B' ; 'A'|] ; [|'C' ; 'B' ; 'C'|]|])

  let solve (start_board_ : board) (end_board_ : board) (mode: mode) (max_steps : int) : unit =
    let start_board = canonical start_board_
      and end_board = canonical end_board_ in
    (* basic_check start_board end_board; *)
    let discovered = Hashtbl.create (max_steps/100) in
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
        (* if debug then print_string (latex_solution l start_board); *)
        raise (Solution l)
      )
      else if !steps < max_steps then (
        incr steps;
        if debug then print_string "Detecting neighbours... ";
        let number_neighbours = ref 0 in
        let number_pieces = number_pieces start_board in
        let the_pieces = pieces start_board in 
        for p = 0 to number_pieces - 1 do
          let current_piece = the_pieces.(p) in
            for d = 0 to number_directions - 1 do
              let current_direction = directions.(d) in
              let (ok, next_board_) = apply (current_piece, current_direction) current_board in
              let next_board = canonical next_board_ in
              if ok && not (Hashtbl.mem discovered next_board)
                && not (Hashtbl.mem discovered (canonical (symetrical next_board))) then (
                Hashtbl.add discovered next_board (Some (current_piece, current_direction));
                Queue.push next_board queue;
                incr number_neighbours;
              );
          done;
        done;
        if debug then print_int (!number_neighbours);
        if debug then print_string "\n";
      ) else (failwith "No solution")
    done;
    failwith "No solution"


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
        if board.(i).(j) == '_' then board.(i).(j) <- ' '
      done;
    done;
    board

end