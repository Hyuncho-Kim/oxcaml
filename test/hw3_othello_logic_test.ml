open! Core
open Othello_logic_library
open Hw2_othello_logic

let ok_exn result = Result.ok result |> Option.value_exn

(* Creates a standard 8x8 Othello board and asserts that it is equal to a manually defined expected state.
   The [%test] extension tells the testing framework to run this function. *)
let%test "Initial Othello board creation" =
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  let expected_board =
    Cell_position.Map.of_alist_exn
      [ { row = 3; column = 3 }, Player_kind.White
      ; { row = 4; column = 4 }, Player_kind.White
      ; { row = 3; column = 4 }, Player_kind.Black
      ; { row = 4; column = 3 }, Player_kind.Black
      ]
  in
  let expected_state : Game_state.t =
    { board = expected_board
    ; rows = 8
    ; columns = 8
    ; decision = In_progress { whose_turn = Black }
    ; last_move = None
    }
  in
  Game_state.equal state expected_state
;;

(* A helper function for "expect tests". It calls the create function and
   prints the resulting S-expression *)
let create_and_print ~rows ~columns =
  let result = Game_state.create ~rows ~columns in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t) Result.t)]
;;


(* An "expect test" using the helper above. It checks both the successful
   creation of a standard board and the expected error cases when the board
   dimensions are invalid. *)
let%expect_test "Game_state.create for Othello" =
  create_and_print ~rows:8 ~columns:8;
  [%expect
    {|
    (Ok
     ((board
       ((((row 3) (column 3)) White) (((row 3) (column 4)) Black)
        (((row 4) (column 3)) Black) (((row 4) (column 4)) White)))
      (rows 8) (columns 8) (decision (In_progress (whose_turn Black)))
      (last_move ())))
    |}];
  create_and_print ~rows:7 ~columns:8;
  [%expect {| (Error Board_must_be_even_and_at_least_4x4) |}];
  create_and_print ~rows:4 ~columns:2;
  [%expect {| (Error Board_must_be_even_and_at_least_4x4) |}]
;;



(* Another helper for expect tests. It applies a move to a given game state
   and prints the S-expression of the result. *)
let make_move_and_print game_state cell_position =
  let result = Game_state.make_move game_state cell_position in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;


(* A convenience value representing the standard 8x8 starting board.
   Used as the initial state for many of the following tests. *)
let initial_8x8 = Game_state.create ~rows:8 ~columns:8 |> ok_exn

(* Verifies that a legal opening move for Black is processed correctly.
   It checks that the new piece is placed, the opponent's piece is flipped,
   and the turn correctly passes to White. *)
let%expect_test "A valid first move for Black" =
  make_move_and_print initial_8x8 { row = 2; column = 3 };
  [%expect
    {|
    (Ok
     ((board
       ((((row 2) (column 3)) Black) (((row 3) (column 3)) Black)
        (((row 3) (column 4)) Black) (((row 4) (column 3)) Black)
        (((row 4) (column 4)) White)))
      (rows 8) (columns 8) (decision (In_progress (whose_turn White)))
      (last_move (((row 2) (column 3))))))
    |}]
;;


(* Checks three different types of invalid moves to ensure
   the logic correctly rejects them. *)
let%expect_test "Game_state.make_move fails for various invalid moves" =
  (* 1. Move is off the board *)
  make_move_and_print initial_8x8 { row = 8; column = 8 };
  [%expect {| (Error Invalid_move) |}];
  (* 2. Move is on a space that is already filled *)
  make_move_and_print initial_8x8 { row = 3; column = 3 };
  [%expect {| (Error Invalid_move) |}];
  (* 3. Move is on an empty space but does not flip any opponent pieces *)
  make_move_and_print initial_8x8 { row = 0; column = 0 };
  [%expect {| (Error Invalid_move) |}]
;;


let pretty_print_board ({ board; rows; columns; decision; _ } : Game_state.t) =
  let black_score, white_score = Game_state.scores { board; rows; columns; decision; last_move=None } in
  print_endline "  0 1 2 3 4 5 6 7";
  for row = 0 to rows - 1 do
    let row_str =
      List.range 0 columns
      |> List.map ~f:(fun column ->
        match Map.find board { row; column } with
        | None -> "."
        | Some player -> String.prefix (Player_kind.to_string player) 1)
      |> String.concat ~sep:" "
    in
    printf "%d %s\n" row row_str
  done;
  printf "Scores: Black %d, White %d\n" black_score white_score;
  print_s [%sexp (decision : Decision.t)]
;;


let print_final_state game_state cell_positions =
  let result =
    List.fold cell_positions ~init:game_state ~f:(fun new_state cell_position ->
      Game_state.make_move new_state cell_position |> ok_exn)
  in
  pretty_print_board result
;;


(* Check that the initial board prints as expected. *)
let%expect_test "Initial Othello board pretty-printed" =
  pretty_print_board initial_8x8;
  [%expect
    {|
      0 1 2 3 4 5 6 7
    0 . . . . . . . .
    1 . . . . . . . .
    2 . . . . . . . .
    3 . . . W B . . .
    4 . . . B W . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 2, White 2
    (In_progress (whose_turn Black))
    |}]
;;


let%expect_test "Board after a few moves" =
  print_final_state
    initial_8x8
    [ { row = 2; column = 3 } (* B *)
    ; { row = 2; column = 2 } (* W *)
    ; { row = 3; column = 2 } (* B *)
    ];
  [%expect
    {|
      0 1 2 3 4 5 6 7
    0 . . . . . . . .
    1 . . . . . . . .
    2 . . W W . . . .
    3 . . B B B . . .
    4 . . . B W . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 4, White 3
    (In_progress (whose_turn White))
    |}]
;;

(*
let%expect_test "Othello game where a player's turn is skipped" =
  (* We'll use a smaller 4x4 board to construct this scenario easily *)
  let initial_4x4 = Game_state.create ~rows:4 ~columns:4 |> ok_exn in
  (*
     Initial 4x4:    B makes move (0,1): B flips W at (1,1)
     . . . .         . B . .
     . W B .         . B B .
     . B W .         . B W .
     . . . .         . . . .
     Now it's White's turn, but White has NO legal moves.
     The turn should be passed back to Black.
  *)
  print_final_state initial_4x4 [ { row = 0; column = 1 } ];
  [%expect
    {|
      0 1 2 3
    0 . B . .
    1 . B B .
    2 . B W .
    3 . . . .
    Scores: Black 4, White 1
    (In_progress (whose_turn Black))
    |}]
;; 

let%expect_test "A small game on a 4x4 board that ends" =
  let initial_4x4 = Game_state.create ~rows:4 ~columns:4 |> ok_exn in
  print_final_state
    initial_4x4
    [ { row = 0; column = 1 } (* B *)
    ; { row = 0; column = 2 } (* B (W passed) *)
    ; { row = 0; column = 3 } (* B (W passed) *)
    ; { row = 1; column = 3 } (* B (W passed) *)
    ; { row = 2; column = 3 } (* B (W passed) *)
    ; { row = 3; column = 3 } (* B (W passed) *)
    ; { row = 3; column = 2 } (* B (W passed) *)
    ; { row = 3; column = 1 } (* B (W passed) *)
    ; { row = 3; column = 0 } (* B (W passed) *)
    ; { row = 2; column = 0 } (* B (W passed) *)
    ; { row = 1; column = 0 } (* B (W passed) *)
    ; { row = 0; column = 0 } (* B (W passed) *)
    ];
  [%expect
    {|
      0 1 2 3
    0 B B B B
    1 B B B B
    2 B B B B
    3 B B B B
    Scores: Black 16, White 0
    (Game_over (winner (Black)))
    |}]
;;

let%expect_test "Game_state.get_all_legal_moves for Othello initial state" =
  let all_moves = Game_state.get_all_legal_moves initial_8x8 Player_kind.Black in
  print_s [%message "All legal moves for Black at start" (all_moves : Move.t list)];
  [%expect
    {|
    ("All legal moves for Black at start"
     (all_moves
      (((row 2) (column 3)) ((row 3) (column 2)) ((row 4) (column 5))
       ((row 5) (column 4)))))
    |}]
;; *)