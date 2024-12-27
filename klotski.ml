let () =
  let mode = Board.Allpieces in

  if (Array.length Sys.argv) = 3 then (
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
    try (Board.bfs start_board end_board mode max_steps)
    with Board.Solution l -> (
      assert (Board.check l start_board end_board mode);
      if Board.debug then print_string (Board.latex_solution l start_board);
      Board.write_file "sol.tex" ("\\documentclass[12pt]{article}
      \\usepackage[french]{babel}
      \\usepackage{multicol}
      \\usepackage[a4paper,top=1.2cm,bottom=1.2cm,left=1.2cm,right=1.2cm,marginparwidth=1.75cm]{geometry}
      \\usepackage{amsmath,tikz-cd}
      \\usepackage{tgpagella}
      \\begin{document}
      \\begin{center}
      {\\huge Solution}
      \\end{center}"^(Board.latex_solution l start_board)^"\\end{document}");
  );
  ) else (
    print_string "Not enough arguments"
  )
(* 
  (** Easy variant of Klotski *)
  let start_board = 
  [|
    [|'A';'A';'B';'B'|];  
    [|'A';'V';'V';'B'|];
    [|'D';'V';'V';'C'|];
    [|'D';'D';'C';'C'|];
    [|'V';'X';'X';'V'|];
    [|'V';'X';'X';'V'|]
  |] in

  let end_board = 
  [|
    [|'A';'A';'B';'B'|];  
    [|'A';'X';'X';'B'|];
    [|'D';'X';'X';'C'|];
    [|'D';'D';'C';'C'|];
    [|'V';'V';'V';'V'|];
    [|'V';'V';'V';'V'|]
  |] in *)

 
