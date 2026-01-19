open Game

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
    Game.print_board start_board ;
    Game.print_board end_board ;
    let max_steps = 100000 in

    let mode = match int_of_string (Sys.argv.(3)) with
      | 0 -> Game.Allpieces
      | 1 -> Game.OnlyX
      | _ -> Game.Shape
    in


    try (Game.solve start_board end_board mode max_steps)
    with Game.Solution l -> 
      Game.write_file ("solution.tex") (Game.simple_latex l start_board)
    | Game.NoSolution -> failwith "No solution"
  )

  else (
    print_string "Not enough arguments"
  )
