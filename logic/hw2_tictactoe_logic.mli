open! Core

module Player_kind : sig
  type t =
    | Black
    | White
  [@@deriving sexp, compare, equal, Hash]

  val opposite : t -> t
  val to_string : t -> string
end

(** Represents a single {row, column} coordinate on the board. *)
module Cell_position : sig
  type t =
    { row : int
    ; column : int
    }
  [@@deriving sexp, compare, hash]

  (* Defines a [Cell_position.Map.t]. *)
  include Comparable.S with type t := t
end

(** A move is the act of placing a piece at a specific cell position. *)
module Move : module type of Cell_position

(** Represents the current status of the game. *)
module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Game_over of { winner : Player_kind.t option } (* [None] indicates a draw *)
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

(** The main module for managing the state of an Othello game. *)
module Game_state : sig
  (** The type representing the complete state of a game at any point. *)
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error : sig
    type t = Board_must_be_even_and_at_least_4x4 [@@deriving sexp, compare]
  end

  val create
    :  rows:int
    -> columns:int
    -> (t, Create_error.t list) Result.t

  module Move_error : sig
    type t =
      | Game_is_over
      | Invalid_move (* Covers placing on an occupied square or not flipping any pieces. *)
    [@@deriving sexp, compare]
  end

  (** [get_all_legal_moves t player] returns a list of all valid moves the
      specified [player] can make in the current game state [t]. *)
  val get_all_legal_moves : t -> Player_kind.t -> Move.t list

  (** [make_move t move] attempts to apply a [move] for the current player.
      If the move is legal, it returns an updated game state.
      If not, it returns a [Move_error.t]. This function also handles turn
      passing and game-over detection automatically. *)
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  (** [scores t] returns the current score as a tuple of (black_pieces, white_pieces). *)
  val scores : t -> int * int

  (** A module containing functions exposed only for testing purposes. *)
  module For_testing : sig
    val all_directions : (int * int) list
    val get_pieces_to_flip : t -> Move.t -> Player_kind.t -> Cell_position.t list
  end
end
