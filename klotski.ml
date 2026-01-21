open Game;;

let () = 
  if (Array.length Sys.argv) = 4 then (
    let ic = open_in (Sys.argv.(1)) in
    let n = in_channel_length ic in
    let content1 = really_input_string ic n in
    close_in ic;

    let ic = open_in (Sys.argv.(2)) in
    let n = in_channel_length ic in
    let content2 = really_input_string ic n in
    close_in ic; 
    
    let start_board = Game.string_to_board content1 in
    let end_board = Game.string_to_board content2 in

    let mode = match int_of_string (Sys.argv.(3)) with
      | 0 -> Game.Allpieces
      | 1 -> Game.OnlyX
      | _ -> Game.Shape
    in

    print_string "\nStart: \n";
    Game.print_board start_board ;
    print_string "Target: \n";
    Game.print_board end_board ;

    print_string ((match mode with 
        Game.Allpieces -> "Classical " 
      | Game.OnlyX -> "Labyrinth " 
      | Game.Shape ->  "Tangram " 
    )^"mode\n\n");



    let max_steps = 100000 in
    try (Game.solve start_board end_board mode max_steps)
    with Game.Solution l -> ( 
        print_string ("Solution found in " ^ (Int.to_string (List.length l) ) ^ " moves!\n");
        Game.write_file ("solution.tex") (Game.simple_latex l start_board))
    | Game.NoSolution -> print_string "No solution"
    | Game.Timeout -> print_string "No solution found (timeout)"
  )

  else (
    print_string "4 arguments needed"
  )
