open Game;;

let () = 
  if (Array.length Sys.argv) = 5 then (
    let ic = open_in (Sys.argv.(1)) in
    let n = in_channel_length ic in
    let start_file = really_input_string ic n in
    close_in ic;

    let ic = open_in (Sys.argv.(2)) in
    let n = in_channel_length ic in
    let end_file = really_input_string ic n in
    close_in ic; 
    
    let start_board = Game.string_to_board start_file in
    let end_board = Game.string_to_board end_file in

    let mode = match int_of_string (Sys.argv.(3)) with
      | 0 -> Game.Allpieces
      | 1 -> Game.OnlyX
      | _ -> Game.Shape
    in

    let output_file = Sys.argv.(4) in

    print_string "\nStart: \n";
    Game.print_board start_board ;
    print_string "Target: \n";
    Game.print_board end_board ;

    print_string ((match mode with 
        Game.Allpieces -> "Classical " 
      | Game.OnlyX -> "Labyrinth " 
      | Game.Shape ->  "Tangram " 
    )^"mode\n\n");



    let max_steps = 10000000 in
    try (Game.solve start_board end_board mode max_steps)
    with Game.Solution l -> ( 
        Printf.printf "Solution found in %s moves!\n" (Int.to_string (List.length l) );
        Game.write_file "solution.tex" (Game.simple_latex l start_board end_board);
        Printf.printf "Written in %s with success!\n" output_file)
    | Game.NoSolution -> print_string "No solution"
    | Game.Timeout -> print_string "No solution found (timeout)"
  )

  else (
    print_string "5 arguments needed"
  )
