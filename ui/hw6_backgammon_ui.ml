open! Core
open Virtual_dom
open! Bonsai.Let_syntax

(* ========================================================================== *)
(* GAME TYPES                                                                 *)
(* ========================================================================== *)

module Definitions = struct
  type player_kind =
    | White
    | Black
  [@@deriving sexp, equal]

  type location =
    | Point of int (* 1..24 *)
    | Bar
    | Off
  [@@deriving sexp, equal]

  type decision =
    | In_progress of { whose_turn : player_kind }
    | Winner of player_kind
  [@@deriving sexp, equal]

  type phase =
    | Roll
    | Move
    | Game_over
  [@@deriving sexp, equal]

  type mode =
    | Pass_and_play
    | Vs_cpu
  [@@deriving sexp, equal]

  type stack =
    { owner : player_kind
    ; count : int
    }
  [@@deriving sexp, equal]

  type board = stack option array
  [@@deriving sexp, equal]

  type game_state =
    { board : board
    ; bar_white : int
    ; bar_black : int
    ; off_white : int
    ; off_black : int
    ; dice : int * int         (* 0 means already used *)
    ; decision : decision
    ; phase : phase
    }
  [@@deriving sexp, equal]

  type move =
    { from_ : location
    ; to_ : location
    ; die : int
    }
  [@@deriving sexp, equal]

  let initial_board : board =
    let b = Array.create ~len:24 None in
    b.(0) <- Some { owner = Black; count = 2 };
    b.(5) <- Some { owner = White; count = 5 };
    b.(7) <- Some { owner = White; count = 3 };
    b.(11) <- Some { owner = Black; count = 5 };
    b.(12) <- Some { owner = White; count = 5 };
    b.(16) <- Some { owner = Black; count = 3 };
    b.(18) <- Some { owner = Black; count = 5 };
    b.(23) <- Some { owner = White; count = 2 };
    b
  ;;

  let initial_state : game_state =
    { board = initial_board
    ; bar_white = 0
    ; bar_black = 0
    ; off_white = 0
    ; off_black = 0
    ; dice = (0, 0)
    ; decision = In_progress { whose_turn = White }
    ; phase = Roll
    }
  ;;
end

open Definitions

(* ========================================================================== *)
(* GAME LOGIC                                                                 *)
(* ========================================================================== *)

module Logic = struct
  let other_player = function
    | White -> Black
    | Black -> White
  ;;

  let dice_list (d1, d2) =
    [ d1; d2 ] |> List.filter ~f:(fun d -> d > 0)
  ;;

  let roll_dice () =
    (Random.int 6 + 1, Random.int 6 + 1)
  ;;

  let current_player (st : game_state) =
    match st.decision with
    | In_progress { whose_turn } -> whose_turn
    | Winner _ -> White
  ;;

  (* For a coordinate system that makes move direction easy:
     - White moves "down" from 24 -> 1, and Off at <= 0
     - Black moves "up" from 1 -> 24, and Off at >= 25
     - White Bar is treated like index 25
     - Black Bar is treated like index 0 *)
  let loc_to_int player loc =
    match loc with
    | Point i -> i
    | Bar -> (match player with White -> 25 | Black -> 0)
    | Off -> (match player with White -> 0 | Black -> 25)
  ;;

  let on_bar_count (st : game_state) (p : player_kind) =
    match p with
    | White -> st.bar_white
    | Black -> st.bar_black
  ;;

  let set_on_bar_count (st : game_state) (p : player_kind) (v : int) =
    match p with
    | White -> { st with bar_white = v }
    | Black -> { st with bar_black = v }
  ;;

  let inc_off (st : game_state) (p : player_kind) =
    match p with
    | White -> { st with off_white = st.off_white + 1 }
    | Black -> { st with off_black = st.off_black + 1 }
  ;;

  let check_winner (st : game_state) : game_state =
    if st.off_white >= 15
    then { st with decision = Winner White; phase = Game_over; dice = (0, 0) }
    else if st.off_black >= 15
    then { st with decision = Winner Black; phase = Game_over; dice = (0, 0) }
    else st
  ;;

  let end_turn (st : game_state) : game_state =
    match st.decision with
    | Winner _ -> st
    | In_progress { whose_turn } ->
      { st with
        decision = In_progress { whose_turn = other_player whose_turn }
      ; phase = Roll
      ; dice = (0, 0)
      }
  ;;

  let is_blocked (board : board) ~(target_point : int) ~(player : player_kind) =
    if target_point < 1 || target_point > 24
    then false
    else (
      match board.(target_point - 1) with
      | Some { owner; count } when (not (equal_player_kind owner player)) && count > 1 -> true
      | _ -> false)
  ;;

  let sources_for_player (st : game_state) (player : player_kind) : location list =
    (* If you have pieces on bar, you must play them first (simplified but standard). *)
    if on_bar_count st player > 0
    then [ Bar ]
    else
      List.init 24 ~f:(fun i -> Point (i + 1))
      |> List.filter ~f:(function
           | Point p ->
             (match st.board.(p - 1) with
              | Some { owner; count } -> count > 0 && equal_player_kind owner player
              | None -> false)
           | _ -> false)
  ;;

  (* Determine which die can realize src->dst. Return the chosen die if possible. *)
  let choose_die_for_move (st : game_state) ~(src : location) ~(dst : location) : int option =
    let player = current_player st in
    let dice = dice_list st.dice in
    let src_i = loc_to_int player src in

    let works die =
      let target_i =
        match player with
        | White -> src_i - die
        | Black -> src_i + die
      in
      match dst with
      | Point p -> target_i = p
      | Off ->
        (match player with
         | White -> target_i <= 0
         | Black -> target_i >= 25)
      | Bar -> false
    in
    (* If bearing off, allow overshoot: die >= needed distance. *)
    let overshoot_ok die =
      match dst with
      | Off ->
        (match player with
         | White ->
           (* need die >= src_i to reach <=0 *)
           die >= src_i
         | Black ->
           (* need die >= 25 - src_i to reach >=25 *)
           die >= (25 - src_i))
      | _ -> false
    in

    match dst with
    | Off ->
      (* prefer the smallest die that can bear off *)
      dice
      |> List.filter ~f:(fun d -> works d || overshoot_ok d)
      |> List.min_elt ~compare:Int.compare
    | _ ->
      dice |> List.find ~f:works
  ;;

  let legal_destinations_for_die (st : game_state) ~(src : location) ~(die : int) : location option =
    let player = current_player st in
    let src_i = loc_to_int player src in
    let target_i =
      match player with
      | White -> src_i - die
      | Black -> src_i + die
    in
    if target_i <= 0 && equal_player_kind player White then Some Off
    else if target_i >= 25 && equal_player_kind player Black then Some Off
    else if target_i >= 1 && target_i <= 24 then Some (Point target_i)
    else None
  ;;

  let get_legal_destinations (st : game_state) (src : location) : location list =
    let player = current_player st in
    let dice = dice_list st.dice in

    (* If on bar, must move bar piece; if not on bar but src is Bar, disallow. *)
    if (not (equal_location src Bar)) && on_bar_count st player > 0
    then []
    else if equal_location src Bar && on_bar_count st player = 0
    then []
    else
      dice
      |> List.filter_map ~f:(fun die ->
           match legal_destinations_for_die st ~src ~die with
           | None -> None
           | Some Bar -> None
           | Some Off -> Some Off
           | Some (Point p) ->
             if is_blocked st.board ~target_point:p ~player then None else Some (Point p))
      |> List.dedup_and_sort ~compare:(fun a b -> Sexp.compare (sexp_of_location a) (sexp_of_location b))
  ;;

  let any_moves_exist (st : game_state) : bool =
    match st.decision with
    | Winner _ -> false
    | In_progress _ ->
      let player = current_player st in
      sources_for_player st player
      |> List.exists ~f:(fun src -> not (List.is_empty (get_legal_destinations st src)))
  ;;

  let consume_die (d1, d2) ~(die : int) =
    if d1 = die then (0, d2)
    else if d2 = die then (d1, 0)
    else (d1, d2)
  ;;

let make_move (st : game_state) (m : move) : (game_state, string) Result.t =
  match st.decision with
  | Winner _ ->
    Error "Game over"

  | In_progress { whose_turn = player } ->
    if not (equal_phase st.phase Move) then
      Error "Not in Move phase (press Roll first)."
    else if on_bar_count st player > 0 && not (equal_location m.from_ Bar) then
      Error "You must move pieces from the bar first."
    else
      let d1, d2 = st.dice in
      if not (List.mem (dice_list (d1, d2)) m.die ~equal:Int.equal) then
        Error "No matching die available."
      else
        let legal = get_legal_destinations st m.from_ in
        if not (List.mem legal m.to_ ~equal:equal_location) then
          Error "Illegal destination."
        else
          let new_board = Array.copy st.board in

          (* remove from source *)
          let st1 =
            match m.from_ with
            | Off -> st
            | Bar ->
              let c = on_bar_count st player in
              set_on_bar_count st player (c - 1)
            | Point p ->
              (match new_board.(p - 1) with
               | Some { owner; count } when count > 0 && equal_player_kind owner player ->
                 if count = 1 then new_board.(p - 1) <- None
                 else new_board.(p - 1) <- Some { owner; count = count - 1 };
                 st
               | _ -> st)
          in

          (* add to destination *)
          let st2 =
            match m.to_ with
            | Bar -> st1
            | Off -> inc_off st1 player
            | Point p ->
              (match new_board.(p - 1) with
               | None ->
                 new_board.(p - 1) <- Some { owner = player; count = 1 };
                 st1
               | Some { owner; count } when equal_player_kind owner player ->
                 new_board.(p - 1) <- Some { owner; count = count + 1 };
                 st1
               | Some { owner; count } when count = 1 ->
                 new_board.(p - 1) <- Some { owner = player; count = 1 };
                 let opp = other_player owner in
                 let opp_bar = on_bar_count st1 opp in
                 set_on_bar_count st1 opp (opp_bar + 1)
               | Some _ -> st1)
          in

          let st3 =
            { st2 with
              board = new_board
            ; dice = consume_die st.dice ~die:m.die
            }
          in

          let st4 = check_winner st3 in
          Ok st4
  ;;

  (* CPU: pick a random legal move (simple but acceptable for HW). *)
  let choose_cpu_move (st : game_state) : move option =
    match st.decision with
    | Winner _ -> None
    | In_progress { whose_turn } ->
      let player = whose_turn in
      let sources = sources_for_player st player in
      let candidates =
        sources
        |> List.concat_map ~f:(fun src ->
             get_legal_destinations st src
             |> List.filter_map ~f:(fun dst ->
                  match choose_die_for_move st ~src ~dst with
                  | None -> None
                  | Some die -> Some { from_ = src; to_ = dst; die }))
      in
      if List.is_empty candidates then None
      else Some (List.random_element_exn candidates)
  ;;

  let roll (st : game_state) : game_state =
    match st.decision with
    | Winner _ -> st
    | In_progress _ ->
      if not (equal_phase st.phase Roll) then st
      else { st with dice = roll_dice (); phase = Move }
  ;;
end

(* ========================================================================== *)
(* UI HELPERS                                                                 *)
(* ========================================================================== *)

let attr k v = Vdom.Attr.create k v
let f = Float.to_string
let i = Int.to_string

let svg_polygon ~points ~fill ~attrs =
  Vdom.Node.create_svg "polygon"
    ~attrs:([ attr "points" points; attr "fill" fill ] @ attrs) []
;;

let svg_circle ?stroke ?(stroke_width = 0) ~cx ~cy ~r ~fill () =
  let attrs =
    [ attr "cx" (f cx)
    ; attr "cy" (f cy)
    ; attr "r" (f r)
    ; attr "fill" fill
    ]
    @
    (match stroke with
     | None -> []
     | Some s -> [ attr "stroke" s; attr "stroke-width" (i stroke_width) ])
  in
  Vdom.Node.create_svg "circle" ~attrs []
;;

let svg_text ~x ~y ~text ~size ~weight ~fill =
  Vdom.Node.create_svg
    "text"
    ~attrs:
      [ attr "x" (f x)
      ; attr "y" (f y)
      ; attr "font-size" (i size)
      ; attr "font-weight" (i weight)
      ; attr "fill" fill
      ]
    [ Vdom.Node.text text ]
;;

type point_geom = { cx : float; start_y : float; dy : float }

let get_point_geom (p : int) : point_geom =
  let bot_start = 690.0 in
  let top_start = 110.0 in
  match p with
  | p when p >= 1 && p <= 6 ->
    { cx = 1170.0 -. (Float.of_int p *. 80.0); start_y = bot_start; dy = -50.0 }
  | p when p >= 7 && p <= 12 ->
    { cx = 510.0 -. (Float.of_int (p - 7) *. 80.0); start_y = bot_start; dy = -50.0 }
  | p when p >= 13 && p <= 18 ->
    { cx = 110.0 +. (Float.of_int (p - 13) *. 80.0); start_y = top_start; dy = 50.0 }
  | _ ->
    { cx = 690.0 +. (Float.of_int (p - 19) *. 80.0); start_y = top_start; dy = 50.0 }
;;

let render_checker_stack ~color ?stroke ~(geom : point_geom) ~(count : int) () =
  if count <= 0 then []
  else
    let max_draw = Int.min count 5 in
    let circles =
      List.init max_draw ~f:(fun k ->
        let cy = geom.start_y +. (Float.of_int k *. geom.dy) in
        svg_circle ?stroke ~stroke_width:4 ~cx:geom.cx ~cy ~r:28.0 ~fill:color ())
    in
    let label =
      if count > 5
      then
        [ svg_text
            ~x:(geom.cx +. 14.0)
            ~y:(geom.start_y +. (Float.of_int 4 *. geom.dy) +. 10.0)
            ~text:(i count)
            ~size:20
            ~weight:700
            ~fill:"#000"
        ]
      else []
    in
    circles @ label
;;

let render_valid_move_hint (geom : point_geom) =
  let cy = geom.start_y +. (2.5 *. geom.dy) in
  (* 注意：可选参数要用 ?stroke:(Some "...") *)
  svg_circle ?stroke:(Some "#00FF00") ~stroke_width:4 ~cx:geom.cx ~cy ~r:16.0 ~fill:"none" ()
;;

let render_board
  (st : game_state)
  (selected : location option)
  (legal_dests : location list)
  (on_click_loc : location -> unit Vdom.Effect.t)
  ~(click_enabled : bool)
  : Vdom.Node.t
  =
  let board_bg =
    Vdom.Node.create_svg "g"
      [ Vdom.Node.create_svg "rect"
          ~attrs:[ attr "width" "1200"; attr "height" "800"; attr "fill" "#6F4E37" ] []
      ; Vdom.Node.create_svg "rect"
          ~attrs:[ attr "x" "40"; attr "y" "40"; attr "width" "1120"; attr "height" "720"; attr "fill" "#e9dfd7" ] []
      ]
  in

  let triangles =
    List.init 24 ~f:(fun idx ->
      let p = idx + 1 in
      let geom = get_point_geom p in
      let is_selected =
        match selected with
        | Some (Point s) -> s = p
        | _ -> false
      in
      let is_legal_dest =
        List.exists legal_dests ~f:(fun loc ->
          match loc with
          | Point d -> d = p
          | _ -> false)
      in

      let points_str =
        if Float.(geom.dy > 0.0)
        then Printf.sprintf "%f,%f %f,%f %f,%f" (geom.cx -. 40.0) 70.0 (geom.cx +. 40.0) 70.0 geom.cx 360.0
        else Printf.sprintf "%f,%f %f,%f %f,%f" (geom.cx -. 40.0) 730.0 (geom.cx +. 40.0) 730.0 geom.cx 440.0
      in
      let fill =
        if is_selected then "#FFFF66"
        else if p % 2 = 1 then "#8b4a3a" else "#f1c27d"
      in

      let checkers =
        match st.board.(idx) with
        | None -> []
        | Some { owner; count } ->
          let color = match owner with White -> "#FFFFCC" | Black -> "#222222" in
          let stroke = match owner with White -> "#000000" | Black -> "#FFFFFF" in
          render_checker_stack ~color ~stroke:stroke ~geom ~count ()
      in

      let hints = if is_legal_dest then [ render_valid_move_hint geom ] else [] in

      let click_zone =
        let base_attrs =
          [ attr "x" (f (geom.cx -. 40.0))
          ; attr "y" (if Float.(geom.dy > 0.0) then "70" else "440")
          ; attr "width" "80"
          ; attr "height" "350"
          ; attr "fill" "white"
          ; attr "fill-opacity" "0.001"  (* 0 会在部分环境里变得“不吃点击” *)
          ; attr "pointer-events" "all"
          ; Vdom.Attr.style (Css_gen.create ~field:"cursor" ~value:"pointer")
          ]
        in
        let attrs =
          if click_enabled then base_attrs @ [ Vdom.Attr.on_click (fun _ -> on_click_loc (Point p)) ] else base_attrs
        in
        Vdom.Node.create_svg "rect" ~attrs []
      in

      Vdom.Node.create_svg "g"
        ([ svg_polygon ~points:points_str ~fill ~attrs:[] ] @ checkers @ hints @ [ click_zone ]))
  in

  let render_bar () =
    let bar_geom_w = { cx = 600.0; start_y = 520.0; dy = 32.0 } in
    let bar_geom_b = { cx = 600.0; start_y = 280.0; dy = -32.0 } in
    let w_checkers = render_checker_stack ~color:"#FFFFCC" ~stroke:"#000" ~geom:bar_geom_w ~count:st.bar_white () in
    let b_checkers = render_checker_stack ~color:"#222222" ~stroke:"#FFF" ~geom:bar_geom_b ~count:st.bar_black () in
    let click_zone =
      let base =
        [ attr "x" "560"; attr "y" "70"; attr "width" "80"; attr "height" "660"
        ; attr "fill" "white"; attr "fill-opacity" "0.001"
        ; attr "pointer-events" "all"
        ]
      in
      let attrs = if click_enabled then base @ [ Vdom.Attr.on_click (fun _ -> on_click_loc Bar) ] else base in
      Vdom.Node.create_svg "rect" ~attrs []
    in
    w_checkers @ b_checkers @ [ click_zone ]
  in

  Vdom.Node.create_svg "svg"
    ~attrs:[ attr "viewBox" "0 0 1200 800"; Vdom.Attr.class_ "board" ]
    ([ board_bg ] @ triangles @ render_bar ())
;;

let button ~label ~on_click ~enabled =
  let attrs =
    [ Vdom.Attr.class_ "nav-btn" ]
    @ (if enabled then [ Vdom.Attr.on_click (fun _ -> on_click) ] else [ Vdom.Attr.disabled ])
  in
  Vdom.Node.button ~attrs [ Vdom.Node.text label ]
;;

(* ========================================================================== *)
(* APP                                                                         *)
(* ========================================================================== *)

let app : Vdom.Node.t Bonsai.Computation.t =
  let%sub mode, set_mode =
    Bonsai.state
      (module struct
        type t = mode [@@deriving sexp, equal]
      end)
      ~default_model:Pass_and_play
  in
  let%sub st, set_st =
    Bonsai.state
      (module struct
        type t = game_state [@@deriving sexp, equal]
      end)
      ~default_model:Definitions.initial_state
  in
  let%sub selected, set_selected =
    Bonsai.state
      (module struct
        type t = location option [@@deriving sexp, equal]
      end)
      ~default_model:None
  in
  let%sub message, set_message =
    Bonsai.state (module String) ~default_model:"Press Roll to start."
  in

  let%arr mode = mode
  and set_mode = set_mode
  and st = st
  and set_st = set_st
  and selected = selected
  and set_selected = set_selected
  and message = message
  and set_message = set_message in

  let player =
    match st.decision with
    | In_progress { whose_turn } -> whose_turn
    | Winner w -> w
  in

  let phase_str =
    match st.phase with
    | Roll -> "Roll"
    | Move -> "Move"
    | Game_over -> "GameOver"
  in

  let player_str = match player with White -> "White" | Black -> "Black" in
  let d1, d2 = st.dice in

  let legal_dests =
    match selected with
    | None -> []
    | Some src -> Logic.get_legal_destinations st src
  in

  let is_cpu_turn =
    match mode, st.decision, st.phase with
    | Vs_cpu, In_progress { whose_turn = Black }, _ -> true
    | _ -> false
  in

  let click_enabled =
    match st.decision with
    | Winner _ -> false
    | In_progress _ ->
      (not is_cpu_turn) && (not (equal_phase st.phase Roll))
  in

  let do_cpu_play_turn =
    (* One button: CPU will (1) Roll if needed (2) make moves until dice used or stuck (3) End turn. *)
    match st.decision with
    | Winner _ -> set_message "Game is over."
    | In_progress { whose_turn } ->
      if not (equal_player_kind whose_turn Black) then set_message "Not CPU's turn."
      else
        let rec loop (acc : game_state) (steps : int) : game_state * string =
          if steps > 50 then acc, "CPU: stopped (safety cap)."
          else if equal_phase acc.phase Roll then
            let acc' = Logic.roll acc in
            if Logic.any_moves_exist acc'
            then loop acc' (steps + 1)
            else (Logic.end_turn acc'), "CPU rolled but had no moves. Turn ended."
          else if equal_phase acc.phase Move then (
            match Logic.choose_cpu_move acc with
            | None ->
              (Logic.end_turn acc), "CPU: no moves, ending turn."
            | Some mv ->
              (match Logic.make_move acc mv with
               | Error _ ->
                 (Logic.end_turn acc), "CPU: move failed unexpectedly, ending turn."
               | Ok acc' ->
                 let acc'' = Logic.check_winner acc' in
                 (match acc''.decision with
                  | Winner _ -> acc'', "CPU finished the game!"
                  | In_progress _ ->
                    let d1', d2' = acc''.dice in
                    if d1' = 0 && d2' = 0
                    then (Logic.end_turn acc''), "CPU used all dice. Turn ended."
                    else loop acc'' (steps + 1))))
          else acc, "CPU: game over."
        in
        let st0 = { st with phase = st.phase } in
        let st', msg = loop st0 0 in
        Vdom.Effect.Many [ set_st st'; set_selected None; set_message msg ]
  in

  let do_new_game =
    Vdom.Effect.Many
      [ set_st Definitions.initial_state
      ; set_selected None
      ; set_message "Press Roll to start."
      ]
  in

  let do_roll =
    match st.decision with
    | Winner _ -> set_message "Game is over. Press New Game."
    | In_progress _ ->
      if not (equal_phase st.phase Roll) then set_message "Already rolled."
      else
        let st' = Logic.roll st in
        if Logic.any_moves_exist st' then
          Vdom.Effect.Many [ set_st st'; set_selected None; set_message "Rolled! Select a piece to move." ]
        else
          let st'' = Logic.end_turn st' in
          let msg = "Rolled, but no moves available. Turn ended." in
          let base = Vdom.Effect.Many [ set_st st''; set_selected None; set_message msg ] in
          match mode, st''.decision with
          | Vs_cpu, In_progress { whose_turn = Black } ->
            Vdom.Effect.Many [ base; do_cpu_play_turn ]
          | _ -> base
  in

  let do_end_turn =
    match st.decision with
    | Winner _ -> set_message "Game is over. Press New Game."
    | In_progress _ ->
      let st' = Logic.end_turn st in
      let base = Vdom.Effect.Many [ set_st st'; set_selected None; set_message "Turn ended. Press Roll." ] in
      match mode, st'.decision with
      | Vs_cpu, In_progress { whose_turn = Black } ->
        Vdom.Effect.Many [ base; do_cpu_play_turn ]
      | _ -> base
  in

let on_click (loc : location) =
  match st.decision with
  | Winner w ->
    set_message
      (Printf.sprintf
         "Winner: %s. Press New Game."
         (match w with
          | White -> "White"
          | Black -> "Black"))

  | In_progress { whose_turn } ->
    if equal_phase st.phase Roll then
      set_message "Press Roll first."
    else if is_cpu_turn then
      set_message "CPU's turn."
    else
      match selected with
      | None ->
        (* selection phase: choose a source *)
        (match loc with
         | Off ->
           set_message "Cannot select Off."
         | Bar ->
           if Logic.on_bar_count st whose_turn > 0 then
             Vdom.Effect.Many
               [ set_selected (Some Bar)
               ; set_message "Selected: Bar."
               ]
           else
             set_message "No pieces on bar."
         | Point p ->
           (match st.board.(p - 1) with
            | Some { owner; count } when count > 0 && equal_player_kind owner whose_turn ->
              Vdom.Effect.Many
                [ set_selected (Some (Point p))
                ; set_message "Piece selected. Choose a highlighted destination."
                ]
            | _ ->
              set_message "Not your piece."))

      | Some src ->
        (* move phase: choose a destination *)
        if equal_location src loc then
          Vdom.Effect.Many
            [ set_selected None
            ; set_message "Deselected."
            ]
        else
          match Logic.choose_die_for_move st ~src ~dst:loc with
          | None ->
            set_message "That destination doesn't match your remaining dice."
          | Some die ->
            let mv = { from_ = src; to_ = loc; die } in
            match Logic.make_move st mv with
            | Error e ->
              set_message ("Invalid move: " ^ e)
            | Ok st' ->
              let st'' = Logic.check_winner st' in
              let msg =
                match st''.decision with
                | Winner w ->
                  Printf.sprintf
                    "Winner: %s"
                    (match w with
                     | White -> "White"
                     | Black -> "Black")
                | In_progress _ ->
                  let d1', d2' = st''.dice in
                  if d1' = 0 && d2' = 0 then
                    "Dice used up. You can End Turn."
                  else
                    "Move ok. Continue."
              in
              Vdom.Effect.Many
                [ set_st st''
                ; set_selected None
                ; set_message msg
                ]
in


  let mode_str = match mode with Pass_and_play -> "Pass-and-play" | Vs_cpu -> "Vs CPU (Black)" in

  let header =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "topbar" ]
      [ Vdom.Node.text
          (Printf.sprintf
             "Mode: %s | Turn: %s | Phase: %s | Dice: %d, %d | Off W/B: %d/%d"
             mode_str
             player_str
             phase_str
             d1
             d2
             st.off_white
             st.off_black)
      ]
  in

  let controls =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "topbar" ]
      [ button ~label:"New Game" ~on_click:do_new_game ~enabled:true
      ; button
          ~label:(if equal_mode mode Pass_and_play then "Switch to Vs CPU" else "Switch to Pass-and-play")
          ~on_click:
            (let new_mode = if equal_mode mode Pass_and_play then Vs_cpu else Pass_and_play in
             Vdom.Effect.Many [ set_mode new_mode; set_selected None; set_message "Mode changed. Press New Game or continue." ])
          ~enabled:true
      ; button ~enabled:(equal_phase st.phase Roll && not (match st.decision with Winner _ -> true | _ -> false))
          ~label:"Roll"
          ~on_click:do_roll
      ; button ~enabled:(equal_phase st.phase Move && not (match st.decision with Winner _ -> true | _ -> false))
          ~label:"End Turn"
          ~on_click:do_end_turn
      ; (match mode, st.decision with
         | Vs_cpu, In_progress { whose_turn = Black } ->
           button ~label:"CPU Play Turn" ~on_click:do_cpu_play_turn ~enabled:true
         | _ -> Vdom.Node.none)
      ]
  in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "page" ]
    [ header
    ; controls
    ; Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.style (Css_gen.create ~field:"color" ~value:"yellow")
          ; Vdom.Attr.style (Css_gen.create ~field:"font-weight" ~value:"bold")
          ]
        [ Vdom.Node.text message ]
    ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "game"; Vdom.Attr.class_ "board-wrap" ]
        [ render_board st selected legal_dests on_click ~click_enabled ]
    ]
;;

let () = Bonsai_web.Start.start app
