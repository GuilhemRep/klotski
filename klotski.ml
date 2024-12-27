let () =
  let mode = Board.Allpieces in

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
  |] in

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
  )