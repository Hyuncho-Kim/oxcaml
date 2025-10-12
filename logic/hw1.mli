(* hw1.mli *)
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

val initial_state : game_state
val move_at_3x2 : move
val state_after_move_at_3x2 : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
