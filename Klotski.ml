open Board;;

if (Array.length Sys.argv) = 4 then (
  let ic = open_in (Sys.argv.(1)) in
  let n = in_channel_length ic in
  let content1 = really_input_string ic n in
  close_in ic;

  let ic = open_in (Sys.argv.(2)) in
  let n = in_channel_length ic in
  let content2 = really_input_string ic n in
  close_in ic; 
  
  let start_board = Board.string_to_board content1 in
  let end_board = Board.string_to_board content2 in
  Board.print_board start_board ;
  Board.print_board end_board ;
  let max_steps = 100000 in

  let mode = match int_of_string (Sys.argv.(3)) with
    | 0 -> Board.Allpieces
    | 1 -> Board.OnlyX
    | _ -> Board.Shape
  in

  try (Board.solve start_board end_board mode max_steps)
  with Board.Solution l -> (
Board.write_file "sol.tex" (Board.simple_latex l start_board);
)
| Board.NoSolution -> failwith "No solution"
)

else (
  print_string "Not enough arguments"
)
