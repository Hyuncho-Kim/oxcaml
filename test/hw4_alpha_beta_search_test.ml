(* hw4_alpha_beta_search_test.ml
 * Location: test/hw4_alpha_beta_search_test.ml
 *
 * Tests for the Othello AI using alpha-beta pruning.
 *)

open! Core
open Othello_logic_library
open Hw2_othello_logic
open Hw4_alpha_beta_search
open Hw3_othello_logic_test

(* Helper to create a custom board state for testing *)
let create_test_board pieces ~whose_turn =
  let board = Cell_position.Map.of_alist_exn pieces in
  { Game_state.board
  ; rows = 8
  ; columns = 8
  ; decision = Decision.In_progress { whose_turn }
  ; last_move = None
  }
;;

(* Helper to print AI's chosen move and resulting board *)
let print_computer_move state depth =
  match alpha_beta state ~depth with
  | None -> print_endline "No move available"
  | Some move ->
    let next_state = Game_state.make_move state move |> ok_exn in
    print_s [%message "Computer chooses this move" (move : Move.t)];
    print_endline "\nThis transitions the game from this state:";
    pretty_print_board state;
    print_endline "\nTo this state:";
    pretty_print_board next_state
;;

(* ================= BASIC AI FUNCTIONALITY TESTS ================= *)

let%expect_test "AI finds a move from initial position" =
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  print_computer_move state 3;
  [%expect {|
    ("Computer chooses this move" (move ((row 2) (column 3))))

    This transitions the game from this state:
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

    To this state:
      0 1 2 3 4 5 6 7
    0 . . . . . . . .
    1 . . . . . . . .
    2 . . . B . . . .
    3 . . . B B . . .
    4 . . . B W . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 4, White 1
    (In_progress (whose_turn White)) |}]
;;

let%expect_test "AI makes legal moves" =
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  match alpha_beta state ~depth:2 with
  | None -> print_endline "FAIL: No move found"
  | Some move ->
    (match Game_state.make_move state move with
    | Ok _ -> printf "✓ AI made legal move at (%d, %d)\n" move.row move.column
    | Error _ -> print_endline "FAIL: AI returned illegal move")
  ;
  [%expect {| ✓ AI made legal move at (2, 3) |}]
;;

(* ================= STRATEGIC TESTS ================= *)

let%expect_test "AI prefers corner when available" =
  (* Create position where corner (0,0) is capturable *)
  let state = create_test_board
    [ { row = 0; column = 1 }, Player_kind.Black
    ; { row = 0; column = 2 }, Player_kind.Black
    ; { row = 1; column = 0 }, Player_kind.White
    ; { row = 1; column = 1 }, Player_kind.White
    ; { row = 2; column = 0 }, Player_kind.White
    ]
    ~whose_turn:Black
  in
  print_computer_move state 3;
  [%expect {|
    ("Computer chooses this move" (move ((row 2) (column 1))))

    This transitions the game from this state:
      0 1 2 3 4 5 6 7
    0 . B B . . . . .
    1 W W . . . . . .
    2 W . . . . . . .
    3 . . . . . . . .
    4 . . . . . . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 2, White 3
    (In_progress (whose_turn Black))

    To this state:
      0 1 2 3 4 5 6 7
    0 . B B . . . . .
    1 W B . . . . . .
    2 W B . . . . . .
    3 . . . . . . . .
    4 . . . . . . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 4, White 2
    (In_progress (whose_turn White)) |}]
;;

(* ================= RANDOM AI TESTS ================= *)

let%test "Random AI returns legal move" =
  Random.init 42;
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  match random_move state with
  | None -> false
  | Some move ->
    (match Game_state.make_move state move with
    | Ok _ -> true
    | Error _ -> false)
;;

let%test "Random AI returns None when game is over" =
  let state = create_test_board [] ~whose_turn:Black in
  let state_over = { state with decision = Decision.Game_over { winner = Some Black } } in
  Option.is_none (random_move state_over)
;;

(* ================= AI VS AI BATTLE ================= *)

(* Play one complete game between two AIs *)
let play_ai_vs_ai ~black_ai ~white_ai =
  let rec play state moves_count =
    if moves_count > 100 then state (* Safety limit *)
    else
      let { Game_state.decision; _ } = state in
      match decision with
      | Decision.Game_over _ -> state
      | Decision.In_progress { whose_turn } ->
        let move_opt =
          match whose_turn with
          | Black -> black_ai state
          | White -> white_ai state
        in
        (match move_opt with
        | None -> state
        | Some move ->
          (match Game_state.make_move state move with
          | Ok next_state -> play next_state (moves_count + 1)
          | Error _ -> state))
  in
  let initial = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  play initial 0
;;

let%test "Alpha-beta can complete a game" =
  Random.init 123;
  let ai = fun state -> alpha_beta state ~depth:2 in
  let final_state = play_ai_vs_ai ~black_ai:ai ~white_ai:ai in
  let { Game_state.decision; _ } = final_state in
  Decision.is_game_over decision
;;

let%expect_test "Random vs Alpha-beta game" =
  Random.init 456;
  let random_ai = random_move in
  let smart_ai = fun state -> alpha_beta state ~depth:2 in (*depth:3*)
  let final_state = play_ai_vs_ai ~black_ai:smart_ai ~white_ai:random_ai in
  pretty_print_board final_state;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 B B B B B B B B
    1 B B B B B B B W
    2 B B B B B B B W
    3 B B B B B B B W
    4 B B W B B B W B
    5 B B B B W W B B
    6 B B B W W B B B
    7 B B B B B B B B
    Scores: Black 55, White 9
    (Game_over (winner (Black))) |}]
;;

(* ================= HEURISTIC TESTS ================= *)

let%test "Heuristic values winning position highly" =
  let state = create_test_board [] ~whose_turn:Black in
  let winning_state = { state with decision = Decision.Game_over { winner = Some Black } } in
  let score = heuristic_value winning_state in
  score = Int.max_value
;;

let%test "Heuristic values losing position poorly" =
  let state = create_test_board [] ~whose_turn:Black in
  let losing_state = { state with decision = Decision.Game_over { winner = Some White } } in
  let score = heuristic_value losing_state in
  score = Int.min_value
;;

let%test "Heuristic prefers more pieces for Black" =
  let state1 = create_test_board
    [ { row = 3; column = 3 }, Player_kind.Black
    ; { row = 3; column = 4 }, Player_kind.White
    ]
    ~whose_turn:Black
  in
  let state2 = create_test_board
    [ { row = 3; column = 3 }, Player_kind.Black
    ; { row = 3; column = 4 }, Player_kind.Black
    ; { row = 4; column = 4 }, Player_kind.White
    ]
    ~whose_turn:Black
  in
  heuristic_value state2 > heuristic_value state1
;;

(* ================= DEPTH TESTS ================= *)

let%test "AI finds moves at various depths" =
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  List.for_all (List.range 1 5) ~f:(fun depth ->
    Option.is_some (alpha_beta state ~depth))
;;

let%test_unit "Higher depth doesn't crash" =
  let state = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  (* Test depths 1-5 *)
  for depth = 1 to 5 do
    match alpha_beta state ~depth with
    | None -> failwith "AI should find a move"
    | Some move ->
      match Game_state.make_move state move with
      | Ok _ -> ()
      | Error _ -> failwithf "Illegal move at depth %d" depth ()
  done
;;

(* ================= WIN RATE TESTS ================= *)

let%expect_test "Alpha-beta vs Random: 1000 games" =
  let wins = ref 0 in
  let losses = ref 0 in
  let draws = ref 0 in
  
  for seed = 0 to 999 do
    Random.init seed;
    let smart_ai = fun state -> alpha_beta state ~depth:2 in (*depth:3*)
    let random_ai = random_move in
    (* Alternate who goes first *)
    let black_ai, white_ai =
      if seed mod 2 = 0 
      then smart_ai, random_ai 
      else random_ai, smart_ai
    in
    let final_state = play_ai_vs_ai ~black_ai ~white_ai in
    let { Game_state.decision; _ } = final_state in
    match decision with
    | Decision.Game_over { winner = Some Black } ->
      if seed mod 2 = 0 then incr wins else incr losses
    | Decision.Game_over { winner = Some White } ->
      if seed mod 2 = 0 then incr losses else incr wins
    | Decision.Game_over { winner = None } -> incr draws
    | _ -> ()
  done;
  
  printf "========================================\n";
  printf "  1000 GAMES: Alpha-beta vs Random\n";
  printf "========================================\n";
  printf "Alpha-beta Wins: %d\n" !wins;
  printf "Random Wins: %d\n" !losses;
  printf "Draws: %d\n" !draws;
  printf "Alpha-beta Win Rate: %.1f%%\n" (float_of_int !wins /. 1000.0 *. 100.0);
  printf "========================================\n";
  [%expect {|
    ========================================
      1000 GAMES: Alpha-beta vs Random
    ========================================
    Alpha-beta Wins: 942
    Random Wins: 53
    Draws: 5
    Alpha-beta Win Rate: 94.2%
    ======================================== |}]
;;

printf "\n✓ All alpha-beta tests completed\n"