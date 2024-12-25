exception Invalid_move
exception Timeout
exception Terminate
val debug : bool
val unit_test : bool
type piece = char
type direction = N | S | E | W
val directions : direction array
val number_directions : int
type board = piece array array
type move = piece * direction
exception Solution of move list
type mode = Allpieces | OnlyX
val mode : mode
val init_board : board
val end_board : board
val init_board : board
val end_board : board
val init_board : board
val end_board : board
val size : int * int
val pieces : piece array
val number_pieces : int
val new_board : unit -> board
val is_equal : board -> board -> bool
val copy : board -> board
val is_finished : board -> bool
val print_board : board -> unit
val move2string : direction -> string
val print_move : char * direction -> unit
val print_move_list : (char * direction) list -> unit
val piece_to_color : char -> string
val generate_tikz : board -> string
val apply : move -> board -> bool * board
val check : move list -> board -> board -> bool
val latex_solution : move list -> board -> string
val opposite : direction -> direction
val bfs : board -> int -> unit
