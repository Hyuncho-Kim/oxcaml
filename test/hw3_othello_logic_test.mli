open! Core
open Othello_logic_library
open Hw2_othello_logic

(** [ok_exn result] returns the value from an [Ok] result, or raises an
    exception if the result is an [Error]. *)
val ok_exn : ('a, 'b) result -> 'a

(** [pretty_print_board game_state] prints a human-readable representation
    of the current board state, scores, and decision to standard output. *)
val pretty_print_board : Game_state.t -> unit

(* applies a sequence of moves to the game state and prints the resulting board.*)
val print_final_state : Game_state.t -> Move.t list -> unit

(*return random legal move for given player*)
val pick_random_move : Game_state.t -> Player_kind.t -> Move.t option

(*play a complete game*)
val random_walk : Game_state.t -> random_seed:int -> unit
