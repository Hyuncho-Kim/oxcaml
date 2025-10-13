(* hw4_alpha_beta_search.ml *)

open! Core
open Hw2_othello_logic

(* Heuristic function to evaluate a board position.
 * Returns a score from the perspective of player X (Black).
 * Higher values are better for Black, lower values are better for White.
 *
 * Othello-specific heuristics:
 * - Corners are extremely valuable (weight: 25)
 * - Edges are valuable (weight: 5)
 * - Mobility (number of legal moves) matters (weight: 2)
 * - Piece count matters (weight: 1)
 *)
let heuristic_value (node : Game_state.t) =
  match node.decision with
  | Game_over { winner } ->
    (* Terminal state - return extreme values *)
    (match winner with
     | Some Player_kind.Black -> Int.max_value
     | Some Player_kind.White -> Int.min_value
     | None -> 0 (* Draw *))
  | In_progress _ ->
    let black_score, white_score = Game_state.scores node in
    
    (* Component 1: Piece count difference *)
    let piece_diff = black_score - white_score in
    
    (* Component 2: Corner control *)
    let corners = 
      [ { Cell_position.row = 0; column = 0 }
      ; { row = 0; column = node.columns - 1 }
      ; { row = node.rows - 1; column = 0 }
      ; { row = node.rows - 1; column = node.columns - 1 }
      ]
    in
    let corner_score =
      List.fold corners ~init:0 ~f:(fun acc pos ->
        match Map.find node.board pos with
        | Some Player_kind.Black -> acc + 25
        | Some Player_kind.White -> acc - 25
        | None -> acc)
    in
    
    (* Component 3: Edge control *)
    let is_edge pos =
      pos.Cell_position.row = 0 
      || pos.row = node.rows - 1 
      || pos.column = 0 
      || pos.column = node.columns - 1
    in
    let edge_score =
      Map.fold node.board ~init:0 ~f:(fun ~key:pos ~data:player acc ->
        if not (is_edge pos) then acc
        else match player with
        | Player_kind.Black -> acc + 5
        | Player_kind.White -> acc - 5)
    in
    
    (* Component 4: Mobility (legal moves available) *)
    let black_moves = List.length (Game_state.get_all_legal_moves node Player_kind.Black) in
    let white_moves = List.length (Game_state.get_all_legal_moves node Player_kind.White) in
    let mobility_score = (black_moves - white_moves) * 2 in
    
    (* Combine all components *)
    piece_diff + corner_score + edge_score + mobility_score
;;

(* Get all possible next states from current state, sorted by heuristic value *)
let children node ~(sort_by_whose_turn : Player_kind.t) =
  let compare =
    match sort_by_whose_turn with
    | Black -> Int.descending  (* Black wants high scores *)
    | White -> Int.ascending   (* White wants low scores *)
  in
  let { Game_state.decision; _ } = node in
  match decision with
  | Game_over _ -> []
  | In_progress { whose_turn } ->
    let moves = Game_state.get_all_legal_moves node whose_turn in
    List.filter_map moves ~f:(fun move -> 
      Game_state.make_move node move |> Result.ok)
    (* Sort children by heuristic value for better pruning *)
    |> List.sort ~compare:(Comparable.lift ~f:heuristic_value compare)
;;

(* Alpha-beta pruning minimax algorithm.
 * https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
 *
 * function alpha_beta(node, depth, α, β, maximizing_player) is
 *     if depth == 0 or node is terminal then
 *         return the heuristic value of node
 *     if maximizing_player then
 *         value := −∞
 *         for each child of node do
 *             value := max(value, alpha_beta(child, depth − 1, α, β, FALSE))
 *             if value ≥ β then
 *                 break (* β cutoff *)
 *             α := max(α, value)
 *         return value
 *     else
 *         value := +∞
 *         for each child of node do
 *             value := min(value, alpha_beta(child, depth − 1, α, β, TRUE))
 *             if value ≤ α then
 *                 break (* α cutoff *)
 *             β := min(β, value)
 *         return value
 *)
let rec alpha_beta_value (node : Game_state.t) depth alpha beta =
  match node.decision with
  | In_progress { whose_turn } when depth > 0 ->
    (match whose_turn with
     | Black ->
       (* Maximizing player *)
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.min_value, alpha)
         ~finish:(fun (value, _alpha) -> value)
         ~f:(fun (value, alpha) child ->
           let value = Int.max value (alpha_beta_value child (depth - 1) alpha beta) in
           let alpha = Int.max alpha value in
           if value >= beta then Stop value (* Beta cutoff *)
           else Continue (value, alpha))
     | White ->
       (* Minimizing player *)
       List.fold_until
         (children node ~sort_by_whose_turn:whose_turn)
         ~init:(Int.max_value, beta)
         ~finish:(fun (value, _beta) -> value)
         ~f:(fun (value, beta) child ->
           let value = Int.min value (alpha_beta_value child (depth - 1) alpha beta) in
           let beta = Int.min beta value in
           if value <= alpha then Stop value (* Alpha cutoff *)
           else Continue (value, beta)))
  | _ -> heuristic_value node
;;

(* Main alpha_beta function that returns the best move.
 * Evaluates all legal moves and picks the one with the best score.
 *)
let alpha_beta (node : Game_state.t) ~depth =
  match node.decision with
  | Game_over _ -> None
  | In_progress { whose_turn } ->
    let moves = Game_state.get_all_legal_moves node whose_turn in
    let moves_and_children =
      List.filter_map moves ~f:(fun move ->
        Game_state.make_move node move
        |> Result.ok
        |> Option.map ~f:(fun child -> move, child))
    in
    let moves_and_children_and_values =
      List.map moves_and_children ~f:(fun (move, child) ->
        move, child, alpha_beta_value child (depth - 1) Int.min_value Int.max_value)
    in
    let best_move =
      (match whose_turn with
       | Black -> List.max_elt
       | White -> List.min_elt)
        moves_and_children_and_values
        ~compare:(fun (_move, _child, v1) (_move, _child, v2) -> Int.compare v1 v2)
      |> Option.map ~f:(fun (move, _child, _value) -> move)
    in
    best_move
;;

(* Simple random move picker for comparison *)
let random_move (node : Game_state.t) =
  match node.decision with
  | Game_over _ -> None
  | In_progress { whose_turn } ->
    let moves = Game_state.get_all_legal_moves node whose_turn in
    if List.is_empty moves 
    then None
    else Some (List.nth_exn moves (Random.int (List.length moves)))
;;