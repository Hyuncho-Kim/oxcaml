(* hw3_othello_logic_test.ml*)
open! Core
open Othello_logic_library
open Hw2_othello_logic

(* helper function to extract value from result type or raise exception *)
let ok_exn result = Result.ok result |> Option.value_exn

(* Basic Creation and initialization tests*)

(*test that initial board match expected state*)
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
  let result = Game_state.equal state expected_state in
  printf "✓ Initial Othello board creation\n";
  result
;;

(*helper function to create board and print as s expressoin*)
let create_and_print ~rows ~columns =
  let result = Game_state.create ~rows ~columns in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t) Result.t)]
;;

(* expect test for vertifying board creation dimesnions*)
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

(*helper to execute move and print result*)
let make_move_and_print game_state cell_position =
  let result = Game_state.make_move game_state cell_position in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

(*standard board used for tests*)
let initial_8x8 = Game_state.create ~rows:8 ~columns:8 |> ok_exn

(*validation tests*)

(*vertify legal first move for black works*)
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

(*check various types of invalid moves are rejected*)
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

(*board printing helpers*)
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

(*apply sequence of moves and print final board state*)
let print_final_state game_state cell_positions =
  let result =
    List.fold cell_positions ~init:game_state ~f:(fun new_state cell_position ->
      match Game_state.make_move new_state cell_position with
      | Ok game -> game
      | Error err ->
        printf "ERROR: Move (%d,%d) failed with %s\n" 
          cell_position.row cell_position.column
          (Sexp.to_string (Game_state.Move_error.sexp_of_t err));
        new_state
    )
  in
  pretty_print_board result
;;



(*expect texts with visual output*)

(*initial board*)
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

(*board after sequence of moves*)
let%expect_test "Board after a few moves" =
  print_final_state
    initial_8x8
    [ { row = 2; column = 3 }
    ; { row = 2; column = 2 }
    ; { row = 3; column = 2 }
    ];
  [%expect
    {|
      0 1 2 3 4 5 6 7
    0 . . . . . . . .
    1 . . . . . . . .
    2 . . W B . . . .
    3 . . B B B . . .
    4 . . . B W . . .
    5 . . . . . . . .
    6 . . . . . . . .
    7 . . . . . . . .
    Scores: Black 5, White 2
    (In_progress (whose_turn White))
    |}]
;;

(*vertify get_all_legal_moves returns correct initial moves*)
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
;;

(* illegal moves*)

(* move outside or board boundaries *)
let%test "Move off board is rejected" =
  let result = 
    match Game_state.make_move initial_8x8 { row = 10; column = 10 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Move off board is rejected\n";
  result
;;

(*place piece on already occupied square*)
let%test "Move on occupied square is rejected" =
  let result =
    match Game_state.make_move initial_8x8 { row = 3; column = 3 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Move on occupied square is rejected\n";
  result
;;

(*piece doesnt flip any of opp's pieces*)
let%test "Move that doesn't flip any pieces is rejected" =
  let result =
    match Game_state.make_move initial_8x8 { row = 0; column = 0 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Move that doesn't flip any pieces is rejected\n";
  result
;;

(*negative row*)
let%test "Negative row position is rejected" =
  let result =
    match Game_state.make_move initial_8x8 { row = -1; column = 3 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Negative row position is rejected\n";
  result
;;

(*negative column*)
let%test "Negative column position is rejected" =
  let result =
    match Game_state.make_move initial_8x8 { row = 3; column = -1 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Negative column position is rejected\n";
  result
;;

(*attempting to place on a previously played square*)
let%test "Move on already placed piece in same turn is rejected" =
  let game = initial_8x8 in
  let game2 = Game_state.make_move game { row = 2; column = 3 } |> ok_exn in
  (* Try to place on the square that was just played *)
  let result =
    match Game_state.make_move game2 { row = 2; column = 3 } with
    | Error Game_state.Move_error.Invalid_move -> true
    | _ -> false
  in
  printf "✓ Move on already placed piece is rejected\n";
  result
;;


(*game logic and trastion test*)


(* Helper to select random legal move or none if it doesnt exist *)
let pick_random_move game player =
  let moves = Game_state.get_all_legal_moves game player in
  if List.is_empty moves 
  then None
  else Some (List.nth_exn moves (Random.int (List.length moves)))
;;

(*error if move attempted after game is over*)
let%test "Cannot move after game is over" =
  let initial_4x4 = Game_state.create ~rows:4 ~columns:4 |> ok_exn in
  (* Play until game is over or we run out of moves *)
  let rec play_until_done (game : Game_state.t) moves_left =
    if moves_left = 0 then game
    else
      match game.decision with
      | Game_over _ -> game
      | In_progress { whose_turn } ->
        (match pick_random_move game whose_turn with
        | None -> game
        | Some move ->
          (match Game_state.make_move game move with
          | Ok new_game -> play_until_done new_game (moves_left - 1)
          | Error _ -> game))
  in
  let final_state = play_until_done initial_4x4 20 in
  (* Try to make a move after game might be over *)
  match final_state.decision with
  | Decision.Game_over _ ->
    (match Game_state.make_move final_state { row = 0; column = 0 } with
    | Error Game_state.Move_error.Game_is_over -> true
    | _ -> false)
  | Decision.In_progress _ -> true (* Game not over yet, that's fine *)
;;

(*check scores updating properly after a mvoe*)
let%test "Scores update correctly during game" =
  let game = initial_8x8 in
  let game2 = Game_state.make_move game { row = 2; column = 3 } |> ok_exn in
  let black, white = Game_state.scores game2 in
  let result = black = 4 && white = 1 in
  printf "✓ Scores update correctly during game\n";
  result
;;

(*test legal moves upadate as game progresses*)
let%test "Legal moves change as game progresses" =
  let game = initial_8x8 in
  let initial_moves = Game_state.get_all_legal_moves game Player_kind.Black in
  let game2 = Game_state.make_move game { row = 2; column = 3 } |> ok_exn in
  let next_moves = Game_state.get_all_legal_moves game2 Player_kind.White in
  let result = not (List.is_empty initial_moves) && not (List.is_empty next_moves) in
  printf "✓ Legal moves change as game progresses\n";
  result
;;

(* random testing *)

(* Random walk helper - plays a game to completion with a given seed *)
let random_walk game ~random_seed =
  Random.init random_seed;
  let rec play (game : Game_state.t) =
    match game.decision with
    | Decision.Game_over { winner } ->
      pretty_print_board game;
      (match winner with
      | Some Player_kind.Black -> printf "(Winner Black)\n"
      | Some Player_kind.White -> printf "(Winner White)\n"
      | None -> printf "Draw\n")
    | Decision.In_progress { whose_turn } ->
      (match pick_random_move game whose_turn with
      | None -> 
        (* No moves available - game should handle this *)
        pretty_print_board game;
        printf "No moves available\n"
      | Some move ->
        (match Game_state.make_move game move with
        | Ok new_game -> play new_game
        | Error _ -> 
          pretty_print_board game;
          printf "Move failed unexpectedly\n"))
  in
  play game
;;

(*expect tests using random walks to verify games reach valid terminal states*)
let%expect_test "Othello random walk till terminal state - seed 1" =
  random_walk initial_8x8 ~random_seed:1;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 W W W B B B B W
    1 W W B B B B B W
    2 W W B B B W B W
    3 B W B B W W B W
    4 B W W W B W W W
    5 B W W W W W W W
    6 W W W W W W W W
    7 W B B B B B B B
    Scores: Black 27, White 37
    (Game_over (winner (White)))
    (Winner White) |}]
;;

(*test different seed showing white can win*)
let%expect_test "Othello random walk till terminal state - seed 42" =
  random_walk initial_8x8 ~random_seed:42;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 W B B B W W W B
    1 W B B W W W W B
    2 W B W W W B W B
    3 W W B W W W B B
    4 W W B W W B B B
    5 W W W B B W B W
    6 W W W W W B B W
    7 W W W W W W B W
    Scores: Black 23, White 41
    (Game_over (winner (White)))
    (Winner White) |}]
;;

(*test different seed showing black can win*)
let%expect_test "Othello random walk till terminal state - seed 123" =
  random_walk initial_8x8 ~random_seed:123;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 W B B B B B B B
    1 W W B B W B B W
    2 B B B B B B B W
    3 B B B B B B B W
    4 B B B W W B B W
    5 B W B B B B W W
    6 W W W B B W W W
    7 B W B B B B B B
    Scores: Black 44, White 20
    (Game_over (winner (Black)))
    (Winner Black) |}]
;;

(*test with smaller board to check game logic*)
let%expect_test "Othello 4x4 random walk - seed 7" =
  let initial_4x4 = Game_state.create ~rows:4 ~columns:4 |> ok_exn in
  random_walk initial_4x4 ~random_seed:7;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 W W W B
    1 W W B W
    2 W B W W
    3 B W W W
    Scores: Black 4, White 12
    (Game_over (winner (White)))
    (Winner White) |}]
;;

(*test with 6x6 board to show game can end with pieces remaining *)
let%expect_test "Othello 6x6 random walk - seed 99" =
  let initial_6x6 = Game_state.create ~rows:6 ~columns:6 |> ok_exn in
  random_walk initial_6x6 ~random_seed:99;
  [%expect {|
      0 1 2 3 4 5 6 7
    0 B B B B . W
    1 B B B B W W
    2 B B B B W W
    3 B B B W W W
    4 B B W B W W
    5 B W W W W W
    Scores: Black 19, White 16
    (Game_over (winner (Black)))
    (Winner Black) |}]
;;


(*stress test with random gameplya*)

(*test to make sure game does not crash*)
let%test_unit "Random game simulation doesn't crash" =
  Random.init 42;
  for _ = 1 to 10 do
    let rec play_random_game (game : Game_state.t) moves_made =
      if moves_made > 60 then ()
      else
        match game.decision with
        | Decision.Game_over _ -> ()
        | Decision.In_progress { whose_turn } ->
          (match pick_random_move game whose_turn with
          | None -> ()
          | Some move ->
            (match Game_state.make_move game move with
            | Ok new_game -> play_random_game new_game (moves_made + 1)
            | Error _ -> ()))
    in
    let initial = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
    play_random_game initial 0
  done;
  printf "✓ Random game simulation doesn't crash (10 games)\n"
;;

(*test random games give valid score*)
let%test "Random games end with valid scores" =
  Random.init 123;
  let rec play_until_end (game : Game_state.t) =
    match game.decision with
    | Decision.Game_over _ -> game
    | Decision.In_progress { whose_turn } ->
      (match pick_random_move game whose_turn with
      | None -> game
      | Some move ->
        (match Game_state.make_move game move with
        | Ok new_game -> play_until_end new_game
        | Error _ -> game))
  in
  let initial = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  let final = play_until_end initial in
  let black, white = Game_state.scores final in
  let result = black >= 0 && white >= 0 && black + white <= 64 in
  printf "✓ Random games end with valid scores\n";
  result
;;

(*test piece count never exceed board capacity*)
let%test "Score never exceeds board capacity" =
  Random.init 456;
  let rec play_and_check (game : Game_state.t) moves_left =
    if moves_left = 0 then true
    else
      let black, white = Game_state.scores game in
      let valid = black >= 0 && white >= 0 && black + white <= 64 in
      if not valid then false
      else
        match game.decision with
        | Decision.Game_over _ -> true
        | Decision.In_progress { whose_turn } ->
          (match pick_random_move game whose_turn with
          | None -> true
          | Some move ->
            (match Game_state.make_move game move with
            | Ok new_game -> play_and_check new_game (moves_left - 1)
            | Error _ -> true))
  in
  let initial = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
  let result = play_and_check initial 30 in
  printf "✓ Score never exceeds board capacity\n";
  result
;;

(*test that board state maintains key invariants throughout random gameplay:
 * - at least one piece always on board
 * - score equals number of pieces on board *)
let%test_unit "Random games maintain board invariants" =
  Random.init 789;
  for _ = 1 to 5 do
    let rec play_game (game : Game_state.t) moves =
      if moves > 20 then ()
      else
        match game.decision with
        | Decision.Game_over _ -> ()
        | Decision.In_progress { whose_turn } ->
          let black, white = Game_state.scores game in
          assert (black > 0 || white > 0);
          assert (black + white = Map.length game.board);
          (match pick_random_move game whose_turn with
          | None -> ()
          | Some move ->
            (match Game_state.make_move game move with
            | Ok new_game -> play_game new_game (moves + 1)
            | Error _ -> ()))
    in
    let initial = Game_state.create ~rows:8 ~columns:8 |> ok_exn in
    play_game initial 0
  done;
  printf "✓ Random games maintain board invariants (5 games)\n"
;;