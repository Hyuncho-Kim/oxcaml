open! Core
open Othello_logic_library
open Hw2_othello_logic
open Virtual_dom
open! Bonsai.Let_syntax

let lookup_cell (game_state : Game_state.t) ~row ~column =
  Map.find game_state.board { row; column }
;;

(* Render the interactive Othello board *)
let othello_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Decision.is_game_over game_state.decision in
  
  let render_cell ~row ~column =
    let cell_value = lookup_cell game_state ~row ~column in
    let is_last_move =
      match game_state.last_move with
      | Some last_move -> Move.equal last_move { row; column }
      | None -> false
    in
    let should_highlight = if is_last_move then [ Vdom.Attr.class_ "highlight" ] else [] in
    let disk_attrs = if is_last_move then [ Vdom.Attr.class_ "slowly_appear" ] else [] in
    
    (* Determine if cell is clickable *)
    let cell_attrs, cell_content =
      match cell_value with
      | Some player ->
        (* Cell has a disk - not clickable *)
        let disk_class =
          match player with
          | Player_kind.Black -> "disk black"
          | Player_kind.White -> "disk white"
        in
        [], [ Vdom.Node.div ~attrs:(Vdom.Attr.class_ disk_class :: disk_attrs) [] ]
      | None when is_game_over ->
        (* Game is over - not clickable *)
        [], []
      | None ->
        (* Empty cell - check if it's a legal move *)
        let current_player =
          match game_state.decision with
          | In_progress { whose_turn } -> Some whose_turn
          | Game_over _ -> None
        in
        (match current_player with
         | None -> [], []
         | Some _ ->
           (* Check if this is a legal move by seeing if it would succeed *)
           let is_legal =
             match Game_state.make_move game_state { row; column } with
             | Ok _ -> true
             | Error _ -> false
           in
           if is_legal
           then (
             (* Legal move - make it clickable *)
             let click_attr =
               Vdom.Attr.(
                 class_ "box-shadow-with-hover-effect"
                 @ on_click (fun _ ->
                   match Game_state.make_move game_state { row; column } with
                   | Error _ -> Ui_effect.Ignore
                   | Ok new_game_state -> set_game_state new_game_state))
             in
             [ click_attr ], [])
           else [], [])
    in
    
    Vdom.Node.div
      ~attrs:([ Vdom.Attr.class_ "cell" ] @ should_highlight @ cell_attrs)
      cell_content
  in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    (List.init game_state.rows ~f:(fun row ->
       Vdom.Node.div
         ~attrs:
           [ Vdom.Attr.class_ "row"
           ; Vdom.Attr.style
               Css_gen.(
                 top
                   (`Percent
                       (Percent.of_percentage
                          (Int.to_float row *. 100.0 /. Int.to_float game_state.rows)))
                 @> height
                      (`Percent
                          (Percent.of_percentage (100.0 /. Int.to_float game_state.rows))))
           ]
         (List.init game_state.columns ~f:(fun column -> render_cell ~row ~column))))
;;

(* Render game info: whose turn, scores, game status *)
let render_game_info (game_state : Game_state.t) =
  let black_score, white_score = Game_state.scores game_state in
  let status_text =
    match game_state.decision with
    | In_progress { whose_turn } ->
      sprintf "%s's turn" (Player_kind.to_string whose_turn)
    | Game_over { winner = Some player } ->
      sprintf "Game Over - %s Wins!" (Player_kind.to_string player)
    | Game_over { winner = None } -> "Game Over - Draw!"
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"text-align" ~value:"center") ]
    [ Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.class_ "state-label"
          ; Vdom.Attr.style (Css_gen.create ~field:"margin-bottom" ~value:"1vh")
          ]
        [ Vdom.Node.text status_text ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "score" ]
        [ Vdom.Node.text (sprintf "Black: %d | White: %d" black_score white_score) ]
    ]
;;

(* Render reset button *)
let render_reset_button ~on_reset =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ -> on_reset)
      ; Vdom.Attr.class_ "reset-button"
      ]
    [ Vdom.Node.text "New Game" ]
;;

(* Main app component *)
let app =
  let initial_state =
    match Game_state.create ~rows:8 ~columns:8 with
    | Ok state -> state
    | Error _ -> failwith "Failed to create initial game state"
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "container game-container" ]
    [ render_game_info game_state
    ; othello_board ~game_state ~set_game_state
    ; render_reset_button ~on_reset:(set_game_state initial_state)
    ]
;;

let () = Bonsai_web.Start.start app