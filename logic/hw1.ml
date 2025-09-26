open! Core

type player_kind =
  | Black
  | White

type cell_position =
  { row : int
  ; column : int
  }

(* A placed disk (position + who owns it) *)
type disk =
  { position : cell_position
  ; owner : player_kind
  }

(* Game status: still playing, someone won, or no moves remain. *)
type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind
  | Stalemate

(* Whole Board: list of placed disk, 8x8, current status *)
type game_state =
  { board : disk list
  ; rows : int
  ; columns : int
  ; decision : decision
  }

(* Choosing a cell to place a disk. *)
type move = cell_position

(*=
Initial board:
........
........
........
...WB...
...BW...
........
........
........
(W = White, B = Black)
*)
let initial_state : game_state =
  { board =
      [ { position = { row = 3; column = 3 }; owner = White }
      ; { position = { row = 4; column = 4 }; owner = White }
      ; { position = { row = 3; column = 4 }; owner = Black }
      ; { position = { row = 4; column = 3 }; owner = Black }
      ]
  ; rows = 8
  ; columns = 8
  ; decision = In_progress { whose_turn = Black }
  }
;;

(*=
1 move from initial board:
........
........
........
..BBB...
...BW...
........
........
........
(W = White, B = Black)
*)
let move_at_3x2 : move = { row = 3; column = 2 }

let state_after_move_at_3x2 : game_state =
  { board =
      [ { position = { row = 3; column = 2 }; owner = Black } (* new move *)
      ; { position = { row = 3; column = 3 }; owner = Black } (* flipped *)
      ; { position = { row = 4; column = 4 }; owner = White }
      ; { position = { row = 3; column = 4 }; owner = Black }
      ; { position = { row = 4; column = 3 }; owner = Black }
      ]
  ; rows = 8
  ; columns = 8
  ; decision = In_progress { whose_turn = White }
  }
;;

(* Before terminal *)
let before_terminal_state : game_state =
  { board =
      [ (* White Pieces *)
        { position = { row = 1; column = 1 }; owner = White }
      ; { position = { row = 2; column = 0 }; owner = White }
      ; { position = { row = 2; column = 6 }; owner = White }
      ; { position = { row = 3; column = 2 }; owner = White }
      ; { position = { row = 3; column = 3 }; owner = White }
      ; { position = { row = 3; column = 5 }; owner = White }
      ; { position = { row = 4; column = 2 }; owner = White }
      ; { position = { row = 4; column = 4 }; owner = White }
      ; { position = { row = 5; column = 2 }; owner = White }
      ; { position = { row = 5; column = 3 }; owner = White }
      ; { position = { row = 6; column = 2 }; owner = White }
      ; { position = { row = 6; column = 4 }; owner = White }
      ; (* Black Pieces *)
        { position = { row = 0; column = 0 }; owner = Black }
      ; { position = { row = 0; column = 1 }; owner = Black }
      ; { position = { row = 0; column = 2 }; owner = Black }
      ; { position = { row = 0; column = 3 }; owner = Black }
      ; { position = { row = 0; column = 4 }; owner = Black }
      ; { position = { row = 0; column = 5 }; owner = Black }
      ; { position = { row = 0; column = 7 }; owner = Black }
      ; { position = { row = 1; column = 0 }; owner = Black }
      ; { position = { row = 1; column = 2 }; owner = Black }
      ; { position = { row = 1; column = 3 }; owner = Black }
      ; { position = { row = 1; column = 4 }; owner = Black }
      ; { position = { row = 1; column = 5 }; owner = Black }
      ; { position = { row = 1; column = 7 }; owner = Black }
      ; { position = { row = 2; column = 1 }; owner = Black }
      ; { position = { row = 2; column = 2 }; owner = Black }
      ; { position = { row = 2; column = 3 }; owner = Black }
      ; { position = { row = 2; column = 4 }; owner = Black }
      ; { position = { row = 2; column = 5 }; owner = Black }
      ; { position = { row = 2; column = 7 }; owner = Black }
      ; { position = { row = 3; column = 0 }; owner = Black }
      ; { position = { row = 3; column = 1 }; owner = Black }
      ; { position = { row = 3; column = 4 }; owner = Black }
      ; { position = { row = 3; column = 6 }; owner = Black }
      ; { position = { row = 3; column = 7 }; owner = Black }
      ; { position = { row = 4; column = 0 }; owner = Black }
      ; { position = { row = 4; column = 1 }; owner = Black }
      ; { position = { row = 4; column = 3 }; owner = Black }
      ; { position = { row = 4; column = 5 }; owner = Black }
      ; { position = { row = 4; column = 6 }; owner = Black }
      ; { position = { row = 4; column = 7 }; owner = Black }
      ; { position = { row = 5; column = 0 }; owner = Black }
      ; { position = { row = 5; column = 1 }; owner = Black }
      ; { position = { row = 5; column = 4 }; owner = Black }
      ; { position = { row = 5; column = 5 }; owner = Black }
      ; { position = { row = 5; column = 6 }; owner = Black }
      ; { position = { row = 5; column = 7 }; owner = Black }
      ; { position = { row = 6; column = 0 }; owner = Black }
      ; { position = { row = 6; column = 1 }; owner = Black }
      ; { position = { row = 6; column = 3 }; owner = Black }
      ; { position = { row = 6; column = 5 }; owner = Black }
      ; { position = { row = 6; column = 6 }; owner = Black }
      ; { position = { row = 6; column = 7 }; owner = Black }
      ; { position = { row = 7; column = 0 }; owner = Black }
      ; { position = { row = 7; column = 1 }; owner = Black }
      ; { position = { row = 7; column = 2 }; owner = Black }
      ; { position = { row = 7; column = 3 }; owner = Black }
      ; { position = { row = 7; column = 4 }; owner = Black }
      ; { position = { row = 7; column = 5 }; owner = Black }
      ; { position = { row = 7; column = 6 }; owner = Black }
      ; { position = { row = 7; column = 7 }; owner = Black }
      ]
  ; rows = 8
  ; columns = 8
  ; decision = In_progress { whose_turn = White }
  }
;;

let move_to_terminal_state : move = { row = 0; column = 6 }

(* terminal *)
let terminal_state : game_state =
  { board =
      [ (* White Pieces *)
        { position = { row = 1; column = 1 }; owner = White }
      ; { position = { row = 1; column = 2 }; owner = White }
      ; { position = { row = 1; column = 3 }; owner = White }
      ; { position = { row = 1; column = 4 }; owner = White }
      ; { position = { row = 1; column = 5 }; owner = White }
      ; { position = { row = 1; column = 6 }; owner = White }
      ; { position = { row = 2; column = 0 }; owner = White }
      ; { position = { row = 2; column = 5 }; owner = White }
      ; { position = { row = 2; column = 6 }; owner = White }
      ; { position = { row = 3; column = 2 }; owner = White }
      ; { position = { row = 3; column = 3 }; owner = White }
      ; { position = { row = 3; column = 4 }; owner = White }
      ; { position = { row = 3; column = 5 }; owner = White }
      ; { position = { row = 4; column = 2 }; owner = White }
      ; { position = { row = 4; column = 3 }; owner = White }
      ; { position = { row = 4; column = 4 }; owner = White }
      ; { position = { row = 5; column = 2 }; owner = White }
      ; { position = { row = 5; column = 3 }; owner = White }
      ; { position = { row = 6; column = 2 }; owner = White }
      ; { position = { row = 6; column = 4 }; owner = White }
      ; (* Black Pieces *)
        { position = { row = 0; column = 0 }; owner = Black }
      ; { position = { row = 0; column = 1 }; owner = Black }
      ; { position = { row = 0; column = 2 }; owner = Black }
      ; { position = { row = 0; column = 3 }; owner = Black }
      ; { position = { row = 0; column = 4 }; owner = Black }
      ; { position = { row = 0; column = 5 }; owner = Black }
      ; { position = { row = 0; column = 7 }; owner = Black }
      ; { position = { row = 1; column = 0 }; owner = Black }
      ; { position = { row = 1; column = 7 }; owner = Black }
      ; { position = { row = 2; column = 1 }; owner = Black }
      ; { position = { row = 2; column = 2 }; owner = Black }
      ; { position = { row = 2; column = 3 }; owner = Black }
      ; { position = { row = 2; column = 4 }; owner = Black }
      ; { position = { row = 2; column = 7 }; owner = Black }
      ; { position = { row = 3; column = 0 }; owner = Black }
      ; { position = { row = 3; column = 1 }; owner = Black }
      ; { position = { row = 3; column = 6 }; owner = Black }
      ; { position = { row = 3; column = 7 }; owner = Black }
      ; { position = { row = 4; column = 0 }; owner = Black }
      ; { position = { row = 4; column = 1 }; owner = Black }
      ; { position = { row = 4; column = 5 }; owner = Black }
      ; { position = { row = 4; column = 6 }; owner = Black }
      ; { position = { row = 4; column = 7 }; owner = Black }
      ; { position = { row = 5; column = 0 }; owner = Black }
      ; { position = { row = 5; column = 1 }; owner = Black }
      ; { position = { row = 5; column = 4 }; owner = Black }
      ; { position = { row = 5; column = 5 }; owner = Black }
      ; { position = { row = 5; column = 6 }; owner = Black }
      ; { position = { row = 5; column = 7 }; owner = Black }
      ; { position = { row = 6; column = 0 }; owner = Black }
      ; { position = { row = 6; column = 1 }; owner = Black }
      ; { position = { row = 6; column = 3 }; owner = Black }
      ; { position = { row = 6; column = 5 }; owner = Black }
      ; { position = { row = 6; column = 6 }; owner = Black }
      ; { position = { row = 6; column = 7 }; owner = Black }
      ; { position = { row = 7; column = 0 }; owner = Black }
      ; { position = { row = 7; column = 1 }; owner = Black }
      ; { position = { row = 7; column = 2 }; owner = Black }
      ; { position = { row = 7; column = 3 }; owner = Black }
      ; { position = { row = 7; column = 4 }; owner = Black }
      ; { position = { row = 7; column = 5 }; owner = Black }
      ; { position = { row = 7; column = 6 }; owner = Black }
      ; { position = { row = 7; column = 7 }; owner = Black }
      ]
  ; rows = 8
  ; columns = 8
  ; decision = Winner Black
  }
;;
