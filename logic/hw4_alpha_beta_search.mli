(* hw4_alpha_beta_search.mli *)

open! Core
open Hw2_othello_logic

(** [heuristic_value node] evaluates the board position.
    Returns an integer score where:
    - High positive values favor Black
    - High negative values favor White
    - Considers piece count, corners, edges, and mobility *)
val heuristic_value : Game_state.t -> int

(** [alpha_beta node ~depth] finds the best move using alpha-beta pruning.
    The [depth] parameter controls search depth (higher = smarter but slower).
    Returns [None] if no legal moves exist or game is over. *)
val alpha_beta : Game_state.t -> depth:int -> Move.t option

(** [random_move node] returns a random legal move.
    This is the trivial AI opponent.
    Returns [None] if no legal moves exist or game is over. *)
val random_move : Game_state.t -> Move.t option