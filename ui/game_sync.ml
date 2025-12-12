open! Core
open Js_of_ocaml
open Othello_logic_library
open Hw2_othello_logic

(* Convert game state to Firestore format *)
let game_state_to_js (game_state : Game_state.t) : < > Js.t =
  let obj = Firebase_bindings.Firestore.create_js_object () in
  
  (* Serialize the board as a JSON string *)
  let board_list = Map.to_alist game_state.board in
  let board_array = Js.array (Array.of_list (List.map board_list ~f:(fun ({ row; column }, player) ->
    let cell_obj = Firebase_bindings.Firestore.create_js_object () in
    Firebase_bindings.Firestore.set_field cell_obj "row" row;
    Firebase_bindings.Firestore.set_field cell_obj "column" column;
    Firebase_bindings.Firestore.set_field cell_obj "player" 
      (Js.string (match player with
        | Player_kind.Black -> "Black"
        | Player_kind.White -> "White"));
    cell_obj
  ))) in
  
  Firebase_bindings.Firestore.set_field obj "board" board_array;
  Firebase_bindings.Firestore.set_field obj "rows" game_state.rows;
  Firebase_bindings.Firestore.set_field obj "columns" game_state.columns;
  
  (* Current turn *)
  let current_turn = match game_state.decision with
    | Decision.In_progress { whose_turn } -> 
      (match whose_turn with
       | Player_kind.Black -> "Black"
       | Player_kind.White -> "White")
    | Decision.Game_over _ -> "GameOver"
  in
  Firebase_bindings.Firestore.set_field obj "currentTurn" (Js.string current_turn);
  
  (* Last move *)
  (match game_state.last_move with
   | Some { row; column } ->
     let last_move_obj = Firebase_bindings.Firestore.create_js_object () in
     Firebase_bindings.Firestore.set_field last_move_obj "row" row;
     Firebase_bindings.Firestore.set_field last_move_obj "column" column;
     Firebase_bindings.Firestore.set_field obj "lastMove" last_move_obj
   | None -> ());
  
  (* Game status *)
  let status = match game_state.decision with
    | Decision.In_progress _ -> "active"
    | Decision.Game_over { winner = Some Player_kind.Black } -> "blackWon"
    | Decision.Game_over { winner = Some Player_kind.White } -> "whiteWon"
    | Decision.Game_over { winner = None } -> "draw"
  in
  Firebase_bindings.Firestore.set_field obj "status" (Js.string status);
  
  obj
;;

(* Convert Firestore data back to game state *)
let js_to_game_state (data : < > Js.t) : Game_state.t =
  let rows = Firebase_bindings.Firestore.get_int_field data "rows" in
  let columns = Firebase_bindings.Firestore.get_int_field data "columns" in
  
  (* Parse board *)
  let board_array = Firebase_bindings.Firestore.get_field data "board" in
  let board_js_array = Js.to_array board_array in
  let board_list = Array.to_list (Array.map board_js_array ~f:(fun cell ->
    let row = Firebase_bindings.Firestore.get_int_field cell "row" in
    let column = Firebase_bindings.Firestore.get_int_field cell "column" in
    let player_str = Firebase_bindings.Firestore.get_string_field cell "player" in
    let player = match player_str with
      | "Black" -> Player_kind.Black
      | "White" -> Player_kind.White
      | _ -> Player_kind.Black
    in
    ({ Cell_position.row; column }, player)
  )) in
  
  let board = Map.of_alist_exn (module Cell_position) board_list in
  
  (* Parse current turn and decision *)
  let current_turn_str = Firebase_bindings.Firestore.get_string_field data "currentTurn" in
  let status_str = Firebase_bindings.Firestore.get_string_field data "status" in
  
  let decision = match status_str with
    | "active" ->
      let whose_turn = match current_turn_str with
        | "Black" -> Player_kind.Black
        | "White" -> Player_kind.White
        | _ -> Player_kind.Black
      in
      Decision.In_progress { whose_turn }
    | "blackWon" -> Decision.Game_over { winner = Some Player_kind.Black }
    | "whiteWon" -> Decision.Game_over { winner = Some Player_kind.White }
    | "draw" -> Decision.Game_over { winner = None }
    | _ -> Decision.In_progress { whose_turn = Player_kind.Black }
  in
  
  (* Parse last move if exists *)
  let last_move = 
    try
      let last_move_obj = Firebase_bindings.Firestore.get_field data "lastMove" in
      let row = Firebase_bindings.Firestore.get_int_field last_move_obj "row" in
      let column = Firebase_bindings.Firestore.get_int_field last_move_obj "column" in
      Some { Move.row; column }
    with _ -> None
  in
  
  { board; rows; columns; decision; last_move }
;;

(* Update game state in Firestore *)
let sync_game_state (game_id : string) (game_state : Game_state.t) : unit =
  let games_collection = Firebase_bindings.Firestore.collection "games" in
  let game_doc = Firebase_bindings.Firestore.doc games_collection game_id in
  let data = game_state_to_js game_state in
  Firebase_bindings.Firestore.set_doc game_doc data
;;

(* Listen for game state changes *)
let listen_to_game (game_id : string) (callback : Game_state.t -> unit) : (unit -> unit) =
  let games_collection = Firebase_bindings.Firestore.collection "games" in
  let game_doc = Firebase_bindings.Firestore.doc games_collection game_id in
  
  Firebase_bindings.Firestore.on_snapshot game_doc (fun doc_opt ->
    Js_of_ocaml.Js.Opt.case doc_opt
      (fun () -> ())
      (fun doc ->
        let data = Firebase_bindings.Firestore.get_data doc in
        let game_state = js_to_game_state data in
        callback game_state
      )
  )
;;

(* Get player color from game *)
let get_player_color (game_id : string) (user_id : string) (callback : Player_kind.t -> unit) : unit =
  let games_collection = Firebase_bindings.Firestore.collection "games" in
  let game_doc = Firebase_bindings.Firestore.doc games_collection game_id in
  
  Firebase_bindings.Firestore.get_doc game_doc (fun doc_opt ->
    Js_of_ocaml.Js.Opt.case doc_opt
      (fun () -> ())
      (fun doc ->
        let data = Firebase_bindings.Firestore.get_data doc in
        let player1_id = Firebase_bindings.Firestore.get_string_field data "player1Id" in
        let player_color = if String.equal player1_id user_id 
          then Player_kind.Black 
          else Player_kind.White 
        in
        callback player_color
      )
  )
;;