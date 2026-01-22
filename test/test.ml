open Game;;

let () = 
    let ic = open_in "../classical/start.txt" in
    let n = in_channel_length ic in
    let content1 = really_input_string ic n in
    close_in ic;

    let ic = open_in "../classical/end.txt" in
    let n = in_channel_length ic in
    let content2 = really_input_string ic n in
    close_in ic; 
    
    let start_board = Game.string_to_board content1 in
    let end_board = Game.string_to_board content2 in
    let max_steps = 200000 in

    let mode = match int_of_string "1" with
      | 0 -> Game.Allpieces
      | 1 -> Game.OnlyX
      | _ -> Game.Shape
    in

    try (Game.solve start_board end_board mode max_steps)
    with Game.Solution l -> 
      assert (List.length l = 112);
    | Game.NoSolution -> failwith "No solution"
    | Game.Timeout -> failwith "No solution found (timeout)"


let () = 
    let ic = open_in "../escape/start.txt" in
    let n = in_channel_length ic in
    let content1 = really_input_string ic n in
    close_in ic;

    let ic = open_in "../escape/end.txt" in
    let n = in_channel_length ic in
    let content2 = really_input_string ic n in
    close_in ic; 
    
    let start_board = Game.string_to_board content1 in
    let end_board = Game.string_to_board content2 in
    let max_steps = 200000 in

    let mode = match int_of_string "0" with
      | 0 -> Game.Allpieces
      | 1 -> Game.OnlyX
      | _ -> Game.Shape
    in

    try (Game.solve start_board end_board mode max_steps)
    with Game.Solution l -> 
      assert (List.length l = 47);
    | Game.NoSolution -> failwith "No solution"
    | Game.Timeout -> failwith "No solution found (timeout)"