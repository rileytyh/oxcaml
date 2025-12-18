open! Core
open Virtual_dom
open! Bonsai.Let_syntax

(* 关键：让 UI 看到 logic library + 你的 HW2 模块 *)
open Backgammon_logic_library
module Logic = Hw2_backgammon_logic

module Player_kind = Logic.Player_kind
module Location = Logic.Location
module Move = Logic.Move
module Decision = Logic.Decision
module Game_state = Logic.Game_state

open Game_state

(* ================================================================ *)
(* UI-side helpers for missing logic functions                      *)
(* ================================================================ *)

let bar_count st p = match p with Player_kind.White -> st.bar_white | Player_kind.Black -> st.bar_black
let off_count st p = match p with Player_kind.White -> st.off_white | Player_kind.Black -> st.off_black

let in_home p pt = match p with Player_kind.White -> 1 <= pt && pt <= 6 | Player_kind.Black -> 19 <= pt && pt <= 24

let all_in_home st p =
  let on_board_ok =
    Array.for_alli st.board ~f:(fun i cell ->
      match cell with
      | None -> true
      | Some s -> if Player_kind.equal s.owner p then in_home p (i + 1) else true)
  in
  on_board_ok && bar_count st p = 0
;;

let destination_point p ~from_pt ~die = match p with
  | Player_kind.White ->
    let to_pt = from_pt - die in
    if to_pt >= 1 then Some to_pt else None
  | Player_kind.Black ->
    let to_pt = from_pt + die in
    if to_pt <= 24 then Some to_pt else None
;;

let enter_from_bar p ~die = match p with
  | Player_kind.White ->
    let pt = 25 - die in
    if 19 <= pt && pt <= 24 then Some pt else None
  | Player_kind.Black ->
    let pt = die in
    if 1 <= pt && pt <= 6 then Some pt else None
;;

(* ================================================================ *)
(* 1) UI 模型：只管“阶段 + 选择 + 错误提示”                            *)
(* ================================================================ *)
module Ui = struct
  type phase =
    | Need_roll
    | Selecting_source
    | Selecting_dest of { src : Location.t }
    | Game_over of { winner : Player_kind.t }
  [@@deriving sexp, equal]

  type t =
    { phase : phase
    ; selected_source : Location.t option
    ; error : string option
    }
  [@@deriving sexp, equal]

  let initial : t = { phase = Need_roll; selected_source = None; error = None }
end

open Ui

(* ================================================================ *)
(* 2) Model = 逻辑状态 + UI 状态                                       *)
(* ================================================================ *)
module Model = struct
  type t =
    { st : Game_state.t
    ; ui : Ui.t
    }
  [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | New_game
    | Roll
    | Click_source of Location.t
    | Click_dest of Location.t
    | End_turn
    | Clear_error
  [@@deriving sexp]
end

(* ================================================================ *)
(* 3) helper：读取 decision / 计算可动与可落点                          *)
(* ================================================================ *)

let sexp_to_string_hum (type a) (sexp_of : a -> Sexp.t) (x : a) =
  Sexp.to_string_hum (sexp_of x)
;;

let set_dice_left (st : Game_state.t) (dice_left : int list) : Game_state.t =
  match st.decision with
  | Decision.Winner _ -> st
  | Decision.In_progress { whose_turn; _ } ->
    { st with decision = Decision.In_progress { whose_turn; dice_left } }
;;

let end_turn (st : Game_state.t) : Game_state.t =
  match st.decision with
  | Decision.Winner _ -> st
  | Decision.In_progress { whose_turn; _ } ->
    { st with
      decision =
        Decision.In_progress
          { whose_turn = Player_kind.opposite whose_turn
          ; dice_left = []
          }
    }
;;

let normalize_new_game (st : Game_state.t) : Game_state.t =
  (* 你的 create() 默认 dice_left=[6;1]，为了 UI 流程一致，清空让用户先 Roll *)
  set_dice_left st []
;;

let get_point_counts (st : Game_state.t) ~(pt : int) : int * int =
  (* 返回 (white_count, black_count) *)
  match st.board.(pt - 1) with
  | None -> 0, 0
  | Some s -> if Player_kind.equal s.owner Player_kind.White then s.count, 0 else 0, s.count
;;

let possible_sources (st : Game_state.t) (p : Player_kind.t) : Location.t list =
  if bar_count st p > 0
  then [ Location.Bar ]
  else (
    List.init 24 ~f:(fun i -> i + 1)
    |> List.filter_map ~f:(fun pt ->
      match st.board.(pt - 1) with
      | Some s when Player_kind.equal s.owner p -> Some (Location.Point pt)
      | _ -> None))
;;

let movable_sources (st : Game_state.t) : Location.t list =
  match st.decision with
  | Decision.Winner _ -> []
  | Decision.In_progress { whose_turn = p; dice_left } ->
    possible_sources st p
    |> List.filter ~f:(fun src ->
      List.exists dice_left ~f:(fun die ->
        Result.is_ok (Game_state.make_move st { Move.from_ = src; die })))
    |> List.dedup_and_sort ~compare:Location.compare
;;

let dest_of (st : Game_state.t) (p : Player_kind.t) ~(src : Location.t) ~(die : int)
  : Location.t option
  =
  match src with
  | Location.Bar ->
    (match enter_from_bar p ~die with
     | None -> None
     | Some pt -> Some (Location.Point pt))
  | Location.Point from_pt ->
    (match destination_point p ~from_pt ~die with
     | Some pt -> Some (Location.Point pt)
     | None ->
       if all_in_home st p && in_home p from_pt
       then Some Location.Off
       else None)
  | Location.Off -> None
;;

let legal_dests_for_source (st : Game_state.t) ~(src : Location.t) : Location.t list =
  match st.decision with
  | Decision.Winner _ -> []
  | Decision.In_progress { whose_turn = p; dice_left } ->
    dice_left
    |> List.filter_map ~f:(fun die ->
      match Game_state.make_move st { Move.from_ = src; die } with
      | Error _ -> None
      | Ok _ -> dest_of st p ~src ~die)
    |> List.dedup_and_sort ~compare:Location.compare
;;

let choose_die_for_dest (st : Game_state.t) ~(src : Location.t) ~(dest : Location.t) : int option
  =
  match st.decision with
  | Decision.Winner _ -> None
  | Decision.In_progress { whose_turn = p; dice_left } ->
    List.find dice_left ~f:(fun die ->
      match dest_of st p ~src ~die with
      | Some d -> Location.equal d dest
      | None -> false)
;;

let roll (st : Game_state.t) : Game_state.t =
  let d1 = 1 + Random.int 6 in
  let d2 = 1 + Random.int 6 in
  let dice_left = if d1 = d2 then [ d1; d1; d1; d1 ] else [ d1; d2 ] in
  set_dice_left st dice_left
;;

(* ================================================================ *)
(* 4) apply_action                                                    *)
(* ================================================================ *)

let apply_action ~inject:_ ~schedule_event:_ (model : Model.t) (action : Action.t) : Model.t =
  let st = model.st in
  let ui = model.ui in
  let clear_sel ui = { ui with selected_source = None } in
  let set_error ui msg = { ui with error = Some msg } in
  let clear_error ui = { ui with error = None } in
  match action with
  | Clear_error -> { model with ui = clear_error ui }
  | New_game ->
    let st0 =
      Game_state.create ()
      |> Result.ok
      |> Option.value_exn
      |> normalize_new_game
    in
    { st = st0; ui = Ui.initial }
  | Roll ->
    (match ui.phase with
     | Ui.Need_roll ->
       let st' = roll st in
       let ui' =
         match st'.decision with
         | Decision.Winner w -> { Ui.initial with phase = Ui.Game_over { winner = w } }
         | Decision.In_progress _ ->
           { (clear_error (clear_sel ui)) with phase = Ui.Selecting_source }
       in
       { st = st'; ui = ui' }
     | _ -> model)
  | End_turn ->
    let st' = end_turn st in
    let ui' = { (clear_error (clear_sel ui)) with phase = Ui.Need_roll } in
    { st = st'; ui = ui' }
  | Click_source src ->
    (match st.decision with
     | Decision.Winner w ->
       { model with ui = { ui with phase = Ui.Game_over { winner = w } } }
     | Decision.In_progress { dice_left = dl; _ } ->
       if List.is_empty dl
       then { model with ui = set_error ui "No dice. Click Roll first." }
       else (
         let mov = movable_sources st in
         if not (List.mem mov src ~equal:Location.equal)
         then { model with ui = set_error ui "That source can't move with current dice." }
         else
           { model with
             ui =
               { (clear_error ui) with
                 phase = Ui.Selecting_dest { src }
               ; selected_source = Some src
               }
           }))
  | Click_dest dest ->
    (match ui.phase with
     | Ui.Selecting_dest { src } ->
       let legal = legal_dests_for_source st ~src in
       if not (List.mem legal dest ~equal:Location.equal)
       then { model with ui = set_error ui "Illegal destination." }
       else (
         match choose_die_for_dest st ~src ~dest with
         | None -> { model with ui = set_error ui "No matching die for that destination." }
         | Some die ->
           (match Game_state.make_move st { Move.from_ = src; die } with
            | Error e ->
              let msg = sexp_to_string_hum [%sexp_of: Game_state.Move_error.t] e in
              { model with ui = set_error ui msg }
            | Ok st' ->
              let ui_base = clear_error (clear_sel ui) in
              let ui' =
                match st'.decision with
                | Decision.Winner w -> { ui_base with phase = Ui.Game_over { winner = w } }
                | Decision.In_progress { dice_left = []; _ } ->
                  { ui_base with phase = Ui.Need_roll }
                | Decision.In_progress _ ->
                  { ui_base with phase = Ui.Selecting_source }
              in
              { st = st'; ui = ui' }))
     | _ -> model)
;;

(* ================================================================ *)
(* 5) view：丑但正确的 UI                                              *)
(* ================================================================ *)

let attr k v = Vdom.Attr.create k v

let button ~(label : string) ~on_click ~(disabled : bool) =
  let attrs =
    [ Vdom.Attr.on_click (fun _ -> on_click ()) ]
    @ if disabled then [ attr "disabled" "true" ] else []
  in
  Vdom.Node.button ~attrs [ Vdom.Node.text label ]
;;

let pill ~(text : string) =
  Vdom.Node.span
    ~attrs:
      [ attr "style"
          "display:inline-block;padding:4px 8px;border:1px solid #999;border-radius:999px;margin-right:6px;"
      ]
    [ Vdom.Node.text text ]
;;

let render_location (loc : Location.t) =
  match loc with
  | Location.Bar -> "BAR"
  | Location.Off -> "OFF"
  | Location.Point pt -> sprintf "P%02d" pt
;;

let view (model : Model.t) ~(inject : Action.t -> unit Vdom.Effect.t) =
  let st = model.st in
  let ui = model.ui in

  let dice_str =
    match st.decision with
    | Decision.Winner _ -> "-"
    | Decision.In_progress { dice_left; _ } ->
      if List.is_empty dice_left
      then "[]"
      else "[" ^ String.concat ~sep:";" (List.map dice_left ~f:Int.to_string) ^ "]"
  in

  let turn_str =
    match st.decision with
    | Decision.Winner _ -> "—"
    | Decision.In_progress { whose_turn; _ } ->
      (match whose_turn with
       | Player_kind.White -> "White"
       | Player_kind.Black -> "Black")
  in

  let phase_str =
    match ui.phase with
    | Ui.Need_roll -> "Need_roll"
    | Ui.Selecting_source -> "Selecting_source"
    | Ui.Selecting_dest _ -> "Selecting_dest"
    | Ui.Game_over _ -> "Game_over"
  in

  let movables = movable_sources st in
  let legal_dests =
    match ui.phase with
    | Ui.Selecting_dest { src } -> legal_dests_for_source st ~src
    | _ -> []
  in
  let is_movable loc = List.mem movables loc ~equal:Location.equal in
  let is_legal_dest loc = List.mem legal_dests loc ~equal:Location.equal in

  let point_cell (pt : int) =
    let loc = Location.Point pt in
    let w, b = get_point_counts st ~pt in
    let selectable = is_movable loc in
    let destable = is_legal_dest loc in
    let selected =
      match ui.selected_source with
      | Some s -> Location.equal s loc
      | None -> false
    in
    let style =
      "padding:10px;border:1px solid #666;border-radius:8px;cursor:pointer;"
      ^ (if selected then "background:#eef;" else "")
      ^ (if selectable then "outline:3px solid #66cc66;" else "")
      ^ (if destable
         then
           "background:repeating-linear-gradient(45deg,#b6f5b6,#b6f5b6 6px,#eaffea 6px,#eaffea 12px);"
         else "")
    in
    Vdom.Node.div
      ~attrs:
        [ attr "style" style
        ; Vdom.Attr.on_click (fun _ ->
            match ui.phase with
            | Ui.Selecting_source -> inject (Action.Click_source loc)
            | Ui.Selecting_dest _ -> inject (Action.Click_dest loc)
            | _ -> Vdom.Effect.Ignore)
        ]
      [ Vdom.Node.text (sprintf "%s  W:%d  B:%d" (render_location loc) w b) ]
  in

  let bar_cell ~(p : Player_kind.t) =
    let loc = Location.Bar in
    let n = bar_count st p in
    let label = match p with Player_kind.White -> sprintf "WHITE BAR: %d" n | Player_kind.Black -> sprintf "BLACK BAR: %d" n in
    let selectable = is_movable loc in
    let selected =
      match ui.selected_source with
      | Some s -> Location.equal s loc
      | None -> false
    in
    let style =
      "padding:10px;border:1px solid #666;border-radius:8px;margin-bottom:8px;cursor:pointer;"
      ^ (if selected then "background:#eef;" else "")
      ^ (if selectable then "outline:3px solid #66cc66;" else "")
    in
    Vdom.Node.div
      ~attrs:
        [ attr "style" style
        ; Vdom.Attr.on_click (fun _ ->
            match ui.phase with
            | Ui.Selecting_source -> inject (Action.Click_source loc)
            | _ -> Vdom.Effect.Ignore)
        ]
      [ Vdom.Node.text label ]
  in

  let off_cell ~(p : Player_kind.t) =
    let loc = Location.Off in
    let n = off_count st p in
    let label = match p with Player_kind.White -> sprintf "WHITE OFF: %d" n | Player_kind.Black -> sprintf "BLACK OFF: %d" n in
    let destable = is_legal_dest loc in
    let style =
      "padding:10px;border:1px solid #666;border-radius:8px;margin-bottom:8px;cursor:pointer;"
      ^ (if destable
         then
           "background:repeating-linear-gradient(45deg,#b6f5b6,#b6f5b6 6px,#eaffea 6px,#eaffea 12px);"
         else "")
    in
    Vdom.Node.div
      ~attrs:
        [ attr "style" style
        ; Vdom.Attr.on_click (fun _ ->
            match ui.phase with
            | Ui.Selecting_dest _ -> inject (Action.Click_dest loc)
            | _ -> Vdom.Effect.Ignore)
        ]
      [ Vdom.Node.text label ]
  in

  let can_roll =
    match ui.phase with
    | Ui.Need_roll -> true
    | _ -> false
  in

  let controls =
    Vdom.Node.div
      ~attrs:[ attr "style" "display:flex;gap:8px;flex-wrap:wrap;margin:12px 0;" ]
      [ button ~label:"New Game" ~disabled:false ~on_click:(fun () -> inject Action.New_game)
      ; button ~label:"Roll" ~disabled:(not can_roll) ~on_click:(fun () -> inject Action.Roll)
      ; button ~label:"End Turn" ~disabled:false ~on_click:(fun () -> inject Action.End_turn)
      ; button ~label:"Clear Error" ~disabled:false ~on_click:(fun () -> inject Action.Clear_error)
      ]
  in

  let status_bar =
    Vdom.Node.div
      ~attrs:[ attr "style" "margin-bottom:10px;" ]
      [ pill ~text:(sprintf "Turn: %s" turn_str)
      ; pill ~text:(sprintf "Dice: %s" dice_str)
      ; pill ~text:(sprintf "Phase: %s" phase_str)
      ]
  in

  let error_box =
    match ui.error with
    | None -> Vdom.Node.none
    | Some e ->
      Vdom.Node.div
        ~attrs:
          [ attr
              "style"
              "padding:10px;border:1px solid #c00;border-radius:8px;color:#c00;margin:10px 0;"
          ]
        [ Vdom.Node.text ("Error: " ^ e) ]
  in

  let game_over_box =
    match st.decision with
    | Decision.Winner w ->
      let wstr = match w with Player_kind.White -> "White" | Player_kind.Black -> "Black" in
      Vdom.Node.div
        ~attrs:[ attr "style" "padding:12px;border:2px solid #000;border-radius:10px;margin:12px 0;" ]
        [ Vdom.Node.text ("Winner: " ^ wstr) ]
    | _ -> Vdom.Node.none
  in

  let board_grid =
    Vdom.Node.div
      ~attrs:
        [ attr
            "style"
            "display:grid;grid-template-columns:repeat(6,1fr);gap:8px;padding:10px;border:1px solid #999;border-radius:12px;"
        ]
      (List.init 24 ~f:(fun i -> point_cell (i + 1)))
  in

  Vdom.Node.div
    ~attrs:[ attr "style" "max-width:900px;margin:0 auto;padding:16px;font-family:system-ui;" ]
    [ status_bar
    ; controls
    ; error_box
    ; game_over_box
    ; Vdom.Node.div
        ~attrs:[ attr "style" "display:grid;grid-template-columns:1fr 2fr;gap:12px;align-items:start;" ]
        [ Vdom.Node.div
            ~attrs:[]
            [ bar_cell ~p:Player_kind.White
            ; bar_cell ~p:Player_kind.Black
            ; off_cell ~p:Player_kind.White
            ; off_cell ~p:Player_kind.Black
            ]
        ; board_grid
        ]
    ]
;;

(* ================================================================ *)
(* 6) Bonsai 组件入口                                                 *)
(* ================================================================ *)

let component : Vdom.Node.t Bonsai.Computation.t =
  let initial_state =
    Game_state.create ()
    |> Result.ok
    |> Option.value_exn
    |> normalize_new_game
  in
  let default_model : Model.t = { st = initial_state; ui = Ui.initial } in
  Bonsai.state_machine0
    (module Model)
    (module Action)
    ~default_model
    ~apply_action
  |> Bonsai.Computation.map ~f:(fun (model, inject) -> view model ~inject)
;;

let () = Bonsai_web.Start.start component
