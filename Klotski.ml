open Board

let () =
  if (Array.length Sys.argv) = 4 then (
    let ic = open_in (Sys.argv.(1)) in
    let n = in_channel_length ic in
    let content1 = really_input_string ic n in
    close_in ic;  (* Close the file *)
    let ic = open_in (Sys.argv.(2)) in
    let n = in_channel_length ic in
    let content2 = really_input_string ic n in
    close_in ic;  (* Close the file *)
    let start_board = Board.string_to_board content1 in
    let end_board = Board.string_to_board content2 in
    Board.print_board start_board ;
    Board.print_board end_board ;
    let max_steps = 100000 in

    let mode = if int_of_string (Sys.argv.(3))==0 then (Board.Allpieces) else (Board.OnlyX) in

    try (Board.bfs start_board end_board mode max_steps)
    with Board.Solution l -> (
      if Board.debug then print_string (Board.latex_solution l start_board);
      (* assert (Board.check l start_board end_board mode); *)
Board.write_file "sol.tex" ("\\documentclass[12pt]{article}
\\usepackage[french]{babel}
\\usepackage{multicol}
\\setlength{\\parindent}{0cm}
\\usepackage[a4paper,top=1.2cm,bottom=1.2cm,left=1.2cm,right=1.2cm,marginparwidth=1.75cm]{geometry}
\\usepackage{amsmath,tikz-cd}
\\usepackage{tgpagella}
\\begin{document}
\\begin{center}
\\end{center}"^(Board.latex_solution l start_board)^"\\end{document}");
  )
  | Board.NoSolution -> failwith "No solution"
  ) else (
    print_string "Not enough arguments"
  )
