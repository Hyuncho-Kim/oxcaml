open! Core
(* hw2_othello_logic.ml *)
module Player_kind = struct
  type t =
    | Black
    | White
  [@@deriving sexp, compare, equal, hash]

  (* It's clearer to use type inference and just write:
     [let opposite t =]
  *)
  let opposite = function
    | Black -> White
    | White -> Black
  ;;

  let to_string = function
    | Black -> "Black"
    | White -> "White"
  ;;
end

module Cell_position = struct
  module T = struct
    type t =
      { row : int
      ; column : int
      }
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

module Move = Cell_position

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Game_over of { winner : Player_kind.t option } (* None for a draw *)
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Game_over _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { board : Player_kind.t Cell_position.Map.t
    ; rows : int
    ; columns : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t = Board_must_be_even_and_at_least_4x4 [@@deriving sexp, compare]
  end

  let create ~rows ~columns : (t, Create_error.t) Result.t =
    if rows % 2 <> 0 || columns % 2 <> 0 || rows < 4 || columns < 4
    then Error Board_must_be_even_and_at_least_4x4
    else (
      let center_r1 = rows / 2 - 1 in
      let center_c1 = columns / 2 - 1 in
      let center_r2 = rows / 2 in
      let center_c2 = columns / 2 in
      let board =
        Cell_position.Map.of_alist_exn
          [ { row = center_r1; column = center_c1 }, Player_kind.White
          ; { row = center_r2; column = center_c2 }, Player_kind.White
          ; { row = center_r1; column = center_c2 }, Player_kind.Black
          ; { row = center_r2; column = center_c1 }, Player_kind.Black
          ]
      in
      Ok
        { board
        ; rows
        ; columns
        ; decision = In_progress { whose_turn = Black }
        ; last_move = None
        })
  ;;

  let is_on_board t (pos : Cell_position.t) =
    pos.row >= 0 && pos.row < t.rows && pos.column >= 0 && pos.column < t.columns
  ;;

  let deltas = List.init 3 ~f:(fun i -> i - 1)

  let all_directions =
    List.cartesian_product deltas deltas
    |> List.filter ~f:(fun (dr, dc) -> dr <> 0 || dc <> 0)
  ;;

  (* Walk in a direction, accumulating opponent pieces. If we hit our own piece,
     return the accumulated list (these are the pieces to flip). Otherwise, return None. *)
  let pieces_to_flip_in_direction t (start_pos : Move.t) player (dr, dc) =
    let opponent = Player_kind.opposite player in
    let rec walk (current_pos : Cell_position.t) pieces_in_between =
      let next_pos : Cell_position.t =
        { row = current_pos.row + dr; column = current_pos.column + dc }
      in
      if not (is_on_board t next_pos)
      then None (* Hit the edge of the board *)
      else (
        match Map.find t.board next_pos with
        | Some p when Player_kind.equal p opponent ->
          walk next_pos (next_pos :: pieces_in_between)
        | Some p when Player_kind.equal p player ->
          Some pieces_in_between (* Found a bracketing piece *)
        | _ -> None (* Found an empty cell or a piece that doesn't form a line *))
    in
    match walk start_pos [] with
    | Some (_ :: _ as pieces) -> Some pieces (* We must flip at least one piece *)
    | _ -> None
  ;;

  let get_pieces_to_flip t move player =
    all_directions
    |> List.filter_map ~f:(fun (dr, dc) ->
      pieces_to_flip_in_direction t move player (dr, dc))
    |> List.concat
  ;;

  let is_legal_move t move player =
    (* First check if the move is on the board *)
    if not (is_on_board t move)
    then false
    else (
      match Map.find t.board move with
    | Some _ -> false (* Space is already filled *)
    | None ->
      (match get_pieces_to_flip t move player with
       | [] -> false (* Move does not flip any pieces *)
       | _ -> true))
    
  ;;

  let get_all_legal_moves t player : Move.t list =
    let rows = List.range 0 t.rows in
    let columns = List.range 0 t.columns in
    List.cartesian_product rows columns
    |> List.map ~f:(fun (row, column) : Move.t -> { row; column })
    |> List.filter ~f:(fun move -> is_legal_move t move player)
  ;;
  
  let scores t =
    Map.fold t.board ~init:(0, 0) ~f:(fun ~key:_ ~data:player (black, white) ->
      match player with
      | Black -> black + 1, white
      | White -> black, white + 1)
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Invalid_move
    [@@deriving sexp, compare]
  end

  let make_move t (move : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Game_over _ -> Error Game_is_over
    | In_progress { whose_turn } ->
      let pieces_to_flip = get_pieces_to_flip t move whose_turn in
      if List.is_empty pieces_to_flip
      then Error Invalid_move
      else (
        (* Apply the move and flip the pieces *)
        let board_with_move = Map.set t.board ~key:move ~data:whose_turn in
        let new_board =
          List.fold pieces_to_flip ~init:board_with_move ~f:(fun board pos ->
            Map.set board ~key:pos ~data:whose_turn)
        in
        let t_after_move = { t with board = new_board; last_move = Some move } in
        
        (* Determine the next turn or end the game *)
        let next_player = Player_kind.opposite whose_turn in
        let next_player_has_moves = not (List.is_empty (get_all_legal_moves t_after_move next_player)) in
        
        let decision =
          if next_player_has_moves
          then Decision.In_progress { whose_turn = next_player }
          else (
            (* Next player must pass; check if the current player can go again *)
            let current_player_has_moves = not (List.is_empty (get_all_legal_moves t_after_move whose_turn)) in
            if current_player_has_moves
            then Decision.In_progress { whose_turn } (* Turn skips back to current player *)
            else (
              (* Neither player can move, so the game is over *)
              let black_score, white_score = scores t_after_move in
              let winner =
                if black_score > white_score then Some Player_kind.Black
                else if white_score > black_score then Some Player_kind.White
                else None (* Draw *)
              in
              Game_over { winner }))
        in
        Ok { t_after_move with decision })
  ;;

  module For_testing = struct
    let all_directions = all_directions
    let get_pieces_to_flip = get_pieces_to_flip
  end
end





(* testing hw2*)
(* let%test_module "Othello Tests" = (module struct
  open Game_state

  let%test "create valid 8x8 board" =
    match create ~rows:8 ~columns:8 with
    | Ok _ -> true
    | Error _ -> false
  ;;

  let%test "reject odd dimensions" =
    match create ~rows:7 ~columns:8 with
    | Ok _ -> false
    | Error Board_must_be_even_and_at_least_4x4 -> true
  ;;

  let%test "reject too small board" =
    match create ~rows:2 ~columns:2 with
    | Ok _ -> false
    | Error Board_must_be_even_and_at_least_4x4 -> true
  ;;

  let%test "initial score is 2-2" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let black, white = scores game in
      black = 2 && white = 2
    | Error _ -> false
  ;;

  let%test "Black goes first" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      (match game.decision with
      | Decision.In_progress { whose_turn = Player_kind.Black } -> true
      | _ -> false)
    | Error _ -> false
  ;;

  let%test "initial legal moves for Black" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let moves = get_all_legal_moves game Player_kind.Black in
      List.length moves = 4
    | Error _ -> false
  ;;

  let%test "legal move is accepted" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 2; column = 3 } in
      (match make_move game move with
      | Ok _ -> true
      | Error _ -> false)
    | Error _ -> false
  ;;

  let%test "move flips correct number of pieces" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 2; column = 3 } in
      (match make_move game move with
      | Ok new_game ->
        let black, white = scores new_game in
        black = 4 && white = 1
      | Error _ -> false)
    | Error _ -> false
  ;;

  let%test "turn switches after move" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 2; column = 3 } in
      (match make_move game move with
      | Ok new_game ->
        (match new_game.decision with
        | Decision.In_progress { whose_turn = Player_kind.White } -> true
        | _ -> false)
      | Error _ -> false)
    | Error _ -> false
  ;;

  let%test "invalid move on occupied square is rejected" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 2; column = 3 } in
      (match make_move game move with
      | Ok new_game ->
        (* Try to place on the same square again *)
        (match make_move new_game move with
        | Ok _ -> false
        | Error Invalid_move -> true
        | Error Game_is_over -> false)
      | Error _ -> false)
    | Error _ -> false
  ;;

  let%test "move off board is rejected" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 10; column = 10 } in
      (match make_move game move with
      | Ok _ -> false
      | Error Invalid_move -> true
      | Error Game_is_over -> false)
    | Error _ -> false
  ;;

  let%test "move that doesn't flip pieces is rejected" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 0; column = 0 } in
      (match make_move game move with
      | Ok _ -> false
      | Error Invalid_move -> true
      | Error Game_is_over -> false)
    | Error _ -> false
  ;;

  let%test "pieces_to_flip finds correct pieces" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move = { Cell_position.row = 2; column = 3 } in
      let pieces = For_testing.get_pieces_to_flip game move Player_kind.Black in
      List.length pieces = 1
      && (match List.hd pieces with
          | Some pos -> pos.row = 3 && pos.column = 3
          | None -> false)
    | Error _ -> false
  ;;

  let%test "all_directions has 8 directions" =
    List.length For_testing.all_directions = 8
  ;;

  let%test "sequence of moves works" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      let move1 = { Cell_position.row = 2; column = 3 } in
      (match make_move game move1 with
      | Ok game2 ->
        let move2 = { Cell_position.row = 2; column = 2 } in
        (match make_move game2 move2 with
        | Ok game3 ->
          let black, white = scores game3 in
          black = 3 && white = 3
        | Error _ -> false)
      | Error _ -> false)
    | Error _ -> false
  ;;

  let%test "is_on_board works correctly" =
    match create ~rows:8 ~columns:8 with
    | Ok game ->
      is_on_board game { Cell_position.row = 0; column = 0 }
      && is_on_board game { Cell_position.row = 7; column = 7 }
      && not (is_on_board game { Cell_position.row = 8; column = 0 })
      && not (is_on_board game { Cell_position.row = -1; column = 0 })
    | Error _ -> false
  ;;
end) *)