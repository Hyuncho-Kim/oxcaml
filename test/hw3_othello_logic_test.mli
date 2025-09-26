open! Core
open Othello_logic_library
open Hw2_othello_logic

(** [ok_exn result] returns the value from an [Ok] result, or raises an
    exception if the result is an [Error]. *)
val ok_exn : ('a, 'b) result -> 'a

(** [pretty_print_board game_state] prints a human-readable representation
    of the current board state, scores, and decision to standard output. *)
val pretty_print_board : Game_state.t -> unit
