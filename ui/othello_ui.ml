open! Core
open Othello_logic_library
open Hw2_othello_logic
open Virtual_dom
open! Bonsai.Let_syntax

let lookup_cell (game_state : Game_state.t) ~row ~column =
  Map.find game_state.board { row; column }
;;

(* Render the Othello board *)
let othello_board ~(game_state : Game_state.t) =
  let render_cell ~row ~column =
    let cell_value = lookup_cell game_state ~row ~column in
    let is_last_move =
      match game_state.last_move with
      | Some last_move -> Move.equal last_move { row; column }
      | None -> false
    in
    let should_highlight = if is_last_move then [ Vdom.Attr.class_ "highlight" ] else [] in
    let disk_attrs = if is_last_move then [ Vdom.Attr.class_ "slowly_appear" ] else [] in
    
    let cell_content =
      match cell_value with
      | Some player ->
        let disk_class =
          match player with
          | Player_kind.Black -> "disk black"
          | Player_kind.White -> "disk white"
        in
        [ Vdom.Node.div ~attrs:(Vdom.Attr.class_ disk_class :: disk_attrs) [] ]
      | None -> []
    in
    Vdom.Node.div
      ~attrs:([ Vdom.Attr.class_ "cell" ] @ should_highlight)
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

(* Render score display *)
let render_score (game_state : Game_state.t) label =
  let black_score, white_score = Game_state.scores game_state in
  let score_text = sprintf "%s - Black: %d | White: %d" label black_score white_score in
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "score" ] [ Vdom.Node.text score_text ]
;;

(* Render a single game state section with label *)
let render_state_section ~game_state ~label =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "state-section" ]
    [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "state-label" ] [ Vdom.Node.text label ]
    ; othello_board ~game_state
    ; render_score game_state ""
    ]
;;

(* Helper to create a board from a list of positions *)
let create_board_from_positions positions =
  Cell_position.Map.of_alist_exn
    (List.map positions ~f:(fun (row, col, player) ->
       { Cell_position.row; column = col }, player))
;;

(* Create the three triplet states *)
let create_triplet_1 () =
  (* Triplet 1: Initial state -> Black plays (3,2) -> After flip *)
  let initial_state = 
    match Game_state.create ~rows:8 ~columns:8 with
    | Ok state -> state
    | Error _ -> failwith "Failed to create initial state"
  in
  
  (* Middle state: showing the move being made - add the disk but don't flip yet *)
  let move_board = Map.set initial_state.board ~key:{ row = 3; column = 2 } ~data:Player_kind.Black in
  let move_state = 
    { initial_state with 
      board = move_board
    ; last_move = Some { row = 3; column = 2 }
    } 
  in
  
  (* After move (3,2) - white disk at (3,3) flips to black - NO last_move so no highlight *)
  let after_move =
    match Game_state.make_move initial_state { row = 3; column = 2 } with
    | Ok state -> { state with last_move = None }
    | Error _ -> failwith "Failed to make move"
  in
  
  initial_state, move_state, after_move
;;

(* Helper to create a game state with custom board *)
let create_custom_game_state ~board ~rows ~columns ~decision ~last_move =
  let base_state = 
    match Game_state.create ~rows ~columns with
    | Ok state -> state
    | Error _ -> failwith "Failed to create base state"
  in
  { base_state with board; decision; last_move }
;;

let create_triplet_2 () =
  (* Triplet 2: Mid-game state from your HW5 image *)
  (* Reading from your image and hw5_static_state2.html *)
  let mid_game_positions =
    [ (* row 0 *)
      (0, 5, Player_kind.White); (0, 6, Player_kind.White); (0, 7, Player_kind.Black)
    ; (* row 1 *)
      (1, 0, Player_kind.White); (1, 1, Player_kind.White); (1, 2, Player_kind.White)
    ; (1, 3, Player_kind.White); (1, 4, Player_kind.White); (1, 5, Player_kind.Black)
    ; (1, 6, Player_kind.Black)
    ; (* row 2 *)
      (2, 1, Player_kind.White); (2, 2, Player_kind.Black); (2, 3, Player_kind.Black)
    ; (2, 4, Player_kind.Black); (2, 5, Player_kind.Black); (2, 6, Player_kind.Black)
    ; (* row 3 *)
      (3, 0, Player_kind.Black); (3, 1, Player_kind.White); (3, 2, Player_kind.White)
    ; (3, 3, Player_kind.White); (3, 4, Player_kind.Black); (3, 5, Player_kind.Black)
    ; (3, 6, Player_kind.Black); (3, 7, Player_kind.Black)
    ; (* row 4 *)
      (4, 0, Player_kind.Black); (4, 1, Player_kind.White); (4, 2, Player_kind.White)
    ; (4, 3, Player_kind.White); (4, 4, Player_kind.White); (4, 5, Player_kind.Black)
    ; (4, 6, Player_kind.Black); (4, 7, Player_kind.Black)
    ; (* row 5 *)
      (5, 0, Player_kind.Black); (5, 1, Player_kind.Black); (5, 2, Player_kind.White)
    ; (5, 3, Player_kind.White); (5, 4, Player_kind.Black); (5, 5, Player_kind.White)
    ; (5, 6, Player_kind.Black); (5, 7, Player_kind.Black)
    ; (* row 6 *)
      (6, 0, Player_kind.Black); (6, 1, Player_kind.Black); (6, 2, Player_kind.White)
    ; (6, 3, Player_kind.White); (6, 6, Player_kind.Black)
    ; (* row 7 *)
      (7, 7, Player_kind.Black)
    ]
  in
  
  let before_state =
    create_custom_game_state
      ~board:(create_board_from_positions mid_game_positions)
      ~rows:8
      ~columns:8
      ~decision:(Decision.In_progress { whose_turn = Player_kind.Black })
      ~last_move:None
  in
  
  (* Middle state: showing move at (7,3) - add disk to board *)
  let move_board = Map.set before_state.board ~key:{ row = 7; column = 3 } ~data:Player_kind.Black in
  let move_state =
    { before_state with
      board = move_board
    ; last_move = Some { row = 7; column = 3 }
    }
  in
  
  (* After state: with flipped disks *)
  let after_positions =
    [ (* row 0 *)
      (0, 5, Player_kind.White); (0, 6, Player_kind.White); (0, 7, Player_kind.Black)
    ; (* row 1 *)
      (1, 0, Player_kind.White); (1, 1, Player_kind.White); (1, 2, Player_kind.White)
    ; (1, 3, Player_kind.White); (1, 4, Player_kind.White); (1, 5, Player_kind.Black)
    ; (1, 6, Player_kind.Black)
    ; (* row 2 *)
      (2, 1, Player_kind.White); (2, 2, Player_kind.Black); (2, 3, Player_kind.Black)
    ; (2, 4, Player_kind.Black); (2, 5, Player_kind.Black); (2, 6, Player_kind.Black)
    ; (* row 3 *)
      (3, 0, Player_kind.Black); (3, 1, Player_kind.White); (3, 2, Player_kind.White)
    ; (3, 3, Player_kind.Black); (3, 4, Player_kind.Black); (3, 5, Player_kind.Black)
    ; (3, 6, Player_kind.Black); (3, 7, Player_kind.Black)
    ; (* row 4 *)
      (4, 0, Player_kind.Black); (4, 1, Player_kind.White); (4, 2, Player_kind.White)
    ; (4, 3, Player_kind.Black); (4, 4, Player_kind.White); (4, 5, Player_kind.Black)
    ; (4, 6, Player_kind.Black); (4, 7, Player_kind.Black)
    ; (* row 5 *)
      (5, 0, Player_kind.Black); (5, 1, Player_kind.Black); (5, 2, Player_kind.White)
    ; (5, 3, Player_kind.Black); (5, 4, Player_kind.Black); (5, 5, Player_kind.White)
    ; (5, 6, Player_kind.Black); (5, 7, Player_kind.Black)
    ; (* row 6 *)
      (6, 0, Player_kind.Black); (6, 1, Player_kind.Black); (6, 2, Player_kind.Black)
    ; (6, 3, Player_kind.Black); (6, 6, Player_kind.Black)
    ; (* row 7 *)
      (7, 3, Player_kind.Black); (7, 7, Player_kind.Black)
    ]
  in
  
  let after_state =
    create_custom_game_state
      ~board:(create_board_from_positions after_positions)
      ~rows:8
      ~columns:8
      ~decision:(Decision.In_progress { whose_turn = Player_kind.White })
      ~last_move:None
  in
  
  before_state, move_state, after_state
;;

let create_triplet_3 () =
  (* Triplet 3: Terminal state from hw5_static_state3.html *)
  (* Before terminal state *)
  let before_positions =
    [ (* row 0 - all black *)
      (0, 0, Player_kind.Black); (0, 1, Player_kind.Black); (0, 2, Player_kind.Black)
    ; (0, 3, Player_kind.Black); (0, 4, Player_kind.Black); (0, 5, Player_kind.Black)
    ; (0, 6, Player_kind.Black); (0, 7, Player_kind.Black)
    ; (* row 1 *)
      (1, 0, Player_kind.Black); (1, 1, Player_kind.White); (1, 2, Player_kind.Black)
    ; (1, 3, Player_kind.Black); (1, 4, Player_kind.Black); (1, 5, Player_kind.Black)
    ; (1, 7, Player_kind.Black)
    ; (* row 2 *)
      (2, 0, Player_kind.White); (2, 1, Player_kind.Black); (2, 2, Player_kind.Black)
    ; (2, 3, Player_kind.Black); (2, 4, Player_kind.Black); (2, 5, Player_kind.Black)
    ; (2, 6, Player_kind.White); (2, 7, Player_kind.Black)
    ; (* row 3 *)
      (3, 0, Player_kind.Black); (3, 1, Player_kind.Black); (3, 2, Player_kind.White)
    ; (3, 3, Player_kind.White); (3, 4, Player_kind.Black); (3, 5, Player_kind.White)
    ; (3, 6, Player_kind.Black); (3, 7, Player_kind.Black)
    ; (* row 4 *)
      (4, 0, Player_kind.Black); (4, 1, Player_kind.Black); (4, 2, Player_kind.White)
    ; (4, 3, Player_kind.Black); (4, 4, Player_kind.White); (4, 5, Player_kind.Black)
    ; (4, 6, Player_kind.Black); (4, 7, Player_kind.Black)
    ; (* row 5 *)
      (5, 0, Player_kind.Black); (5, 1, Player_kind.Black); (5, 2, Player_kind.White)
    ; (5, 3, Player_kind.White); (5, 4, Player_kind.Black); (5, 5, Player_kind.Black)
    ; (5, 6, Player_kind.Black); (5, 7, Player_kind.Black)
    ; (* row 6 *)
      (6, 0, Player_kind.Black); (6, 1, Player_kind.Black); (6, 2, Player_kind.White)
    ; (6, 3, Player_kind.Black); (6, 4, Player_kind.White); (6, 5, Player_kind.Black)
    ; (6, 6, Player_kind.Black); (6, 7, Player_kind.Black)
    ; (* row 7 - all black *)
      (7, 0, Player_kind.Black); (7, 1, Player_kind.Black); (7, 2, Player_kind.Black)
    ; (7, 3, Player_kind.Black); (7, 4, Player_kind.Black); (7, 5, Player_kind.Black)
    ; (7, 6, Player_kind.Black); (7, 7, Player_kind.Black)
    ]
  in
  
  let before_state =
    create_custom_game_state
      ~board:(create_board_from_positions before_positions)
      ~rows:8
      ~columns:8
      ~decision:(Decision.In_progress { whose_turn = Player_kind.White })
      ~last_move:None
  in
  
  (* Move at (1,6) - add disk to board *)
  let move_board = Map.set before_state.board ~key:{ row = 1; column = 6 } ~data:Player_kind.White in
  let move_state =
    { before_state with
      board = move_board
    ; last_move = Some { row = 1; column = 6 }
    }
  in
  
  (* Terminal state - after move *)
  let terminal_positions =
    [ (* row 0 - all black *)
      (0, 0, Player_kind.Black); (0, 1, Player_kind.Black); (0, 2, Player_kind.Black)
    ; (0, 3, Player_kind.Black); (0, 4, Player_kind.Black); (0, 5, Player_kind.Black)
    ; (0, 6, Player_kind.Black); (0, 7, Player_kind.Black)
    ; (* row 1 *)
      (1, 0, Player_kind.Black); (1, 1, Player_kind.White); (1, 2, Player_kind.White)
    ; (1, 3, Player_kind.White); (1, 4, Player_kind.White); (1, 5, Player_kind.White)
    ; (1, 6, Player_kind.White); (1, 7, Player_kind.Black)
    ; (* row 2 *)
      (2, 0, Player_kind.White); (2, 1, Player_kind.Black); (2, 2, Player_kind.Black)
    ; (2, 3, Player_kind.Black); (2, 4, Player_kind.Black); (2, 5, Player_kind.White)
    ; (2, 6, Player_kind.White); (2, 7, Player_kind.Black)
    ; (* row 3 *)
      (3, 0, Player_kind.Black); (3, 1, Player_kind.Black); (3, 2, Player_kind.White)
    ; (3, 3, Player_kind.White); (3, 4, Player_kind.White); (3, 5, Player_kind.White)
    ; (3, 6, Player_kind.Black); (3, 7, Player_kind.Black)
    ; (* row 4 *)
      (4, 0, Player_kind.Black); (4, 1, Player_kind.Black); (4, 2, Player_kind.White)
    ; (4, 3, Player_kind.White); (4, 4, Player_kind.White); (4, 5, Player_kind.Black)
    ; (4, 6, Player_kind.Black); (4, 7, Player_kind.Black)
    ; (* row 5 *)
      (5, 0, Player_kind.Black); (5, 1, Player_kind.Black); (5, 2, Player_kind.White)
    ; (5, 3, Player_kind.White); (5, 4, Player_kind.Black); (5, 5, Player_kind.Black)
    ; (5, 6, Player_kind.Black); (5, 7, Player_kind.Black)
    ; (* row 6 *)
      (6, 0, Player_kind.Black); (6, 1, Player_kind.Black); (6, 2, Player_kind.White)
    ; (6, 3, Player_kind.Black); (6, 4, Player_kind.White); (6, 5, Player_kind.Black)
    ; (6, 6, Player_kind.Black); (6, 7, Player_kind.Black)
    ; (* row 7 - all black *)
      (7, 0, Player_kind.Black); (7, 1, Player_kind.Black); (7, 2, Player_kind.Black)
    ; (7, 3, Player_kind.Black); (7, 4, Player_kind.Black); (7, 5, Player_kind.Black)
    ; (7, 6, Player_kind.Black); (7, 7, Player_kind.Black)
    ]
  in
  
  let terminal_state =
    create_custom_game_state
      ~board:(create_board_from_positions terminal_positions)
      ~rows:8
      ~columns:8
      ~decision:(Decision.Game_over { winner = Some Player_kind.Black })
      ~last_move:None
  in
  
  before_state, move_state, terminal_state
;;

(* Main app component *)
let app =
  Bonsai.const
    (let before_1, move_1, after_1 = create_triplet_1 () in
     let before_2, move_2, after_2 = create_triplet_2 () in
     let before_3, move_3, after_3 = create_triplet_3 () in
     Vdom.Node.div
       ~attrs:[ Vdom.Attr.class_ "container" ]
       [ (* Triplet 1 Row *)
         Vdom.Node.div
           ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"flex"); Vdom.Attr.style (Css_gen.create ~field:"gap" ~value:"2vh") ]
           [ render_state_section ~game_state:before_1 ~label:"Triplet 1: Initial State"
           ; render_state_section ~game_state:move_1 ~label:"Move: Black plays at (3,2)"
           ; render_state_section ~game_state:after_1 ~label:"After Move"
           ]
       ; Vdom.Node.hr ()
       ; (* Triplet 2 Row *)
         Vdom.Node.div
           ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"flex"); Vdom.Attr.style (Css_gen.create ~field:"gap" ~value:"2vh") ]
           [ render_state_section ~game_state:before_2 ~label:"Triplet 2: Mid Game State"
           ; render_state_section ~game_state:move_2 ~label:"Move: Black plays at (7,3)"
           ; render_state_section ~game_state:after_2 ~label:"After Move"
           ]
       ; Vdom.Node.hr ()
       ; (* Triplet 3 Row *)
         Vdom.Node.div
           ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"flex"); Vdom.Attr.style (Css_gen.create ~field:"gap" ~value:"2vh") ]
           [ render_state_section ~game_state:before_3 ~label:"Triplet 3: Before Terminal State"
           ; render_state_section ~game_state:move_3 ~label:"Move: White plays at (1,6)"
           ; render_state_section ~game_state:after_3 ~label:"Terminal State - Black Wins!"
           ]
       ])
;;

let () = Bonsai_web.Start.start app