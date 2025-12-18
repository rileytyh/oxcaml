open! Core
open Virtual_dom
open! Bonsai.Let_syntax

open Backgammon_logic_library
open Hw2_backgammon_logic

(* =============================================================================
   Small helpers
   ============================================================================= *)

let attr k v = Vdom.Attr.create k v
let f (x : float) : string = Printf.sprintf "%g" x
let svg tag ~attrs children = Vdom.Node.create_svg tag ~attrs children

let roll_dice_list () : int list =
  let d1 = Random.int 6 + 1 in
  let d2 = Random.int 6 + 1 in
  if Int.equal d1 d2 then [ d1; d1; d1; d1 ] else [ d1; d2 ]
;;

let bar_count (st : Game_state.t) (p : Player_kind.t) =
  match p with
  | Player_kind.White -> st.bar_white
  | Player_kind.Black -> st.bar_black
;;

let off_count (st : Game_state.t) (p : Player_kind.t) =
  match p with
  | Player_kind.White -> st.off_white
  | Player_kind.Black -> st.off_black
;;

let whose_turn_and_dice (st : Game_state.t) : Player_kind.t option * int list =
  match st.decision with
  | Decision.Winner _ -> None, []
  | Decision.In_progress { whose_turn; dice_left } -> Some whose_turn, dice_left
;;

(* =============================================================================
   UI model
   ============================================================================= *)

module Selected_source = struct
  type t = Location.t option [@@deriving sexp, compare, equal]
end

type phase =
  | Roll_dice
  | Select_source
  | Select_destination
  | Winner
[@@deriving sexp, compare, equal]

let phase_of ~(st : Game_state.t) ~(selected : Location.t option) : phase =
  match st.decision with
  | Decision.Winner _ -> Winner
  | Decision.In_progress { dice_left; _ } ->
    if List.is_empty dice_left
    then Roll_dice
    else (
      match selected with
      | None -> Select_source
      | Some _ -> Select_destination)
;;

(* =============================================================================
   Geometry
   ============================================================================= *)

let point_width = 40.0
let board_height = 300.0
let board_width = (12.0 *. point_width) +. 60.0

let svg_w = board_width +. 80.0
let svg_h = board_height +. 20.0

let board_x = 40.0
let board_y = 10.0

let bar_x = board_x +. (6.0 *. point_width)
let bar_w = point_width

let top_y = board_y
let half_h = (board_height /. 2.0) -. 10.0
let bottom_y = (board_height /. 2.0) +. 20.0

let top_points = List.range 13 25
let bottom_points = List.range 1 13 |> List.rev (* 12..1 left->right *)

let x_offset_for_index i = if i < 6 then i else i + 1

let point_x i =
  let xo = x_offset_for_index i |> Float.of_int in
  board_x +. (xo *. point_width)
;;

type direction =
  | Up
  | Down

let triangle_points ~x ~y ~w ~h ~dir =
  match dir with
  | Down ->
    sprintf "%s,%s %s,%s %s,%s"
      (f x) (f y)
      (f (x +. w)) (f y)
      (f (x +. (w /. 2.0))) (f (y +. h))
  | Up ->
    sprintf "%s,%s %s,%s %s,%s"
      (f x) (f (y +. h))
      (f (x +. w)) (f (y +. h))
      (f (x +. (w /. 2.0))) (f y)
;;

(* =============================================================================
   Move legality (uses Game_state.make_move as truth)
   ============================================================================= *)

let destination_point (p : Player_kind.t) ~(from_pt : int) ~(die : int) : int option =
  match p with
  | Player_kind.White ->
    let to_pt = from_pt - die in
    if to_pt >= 1 then Some to_pt else None
  | Player_kind.Black ->
    let to_pt = from_pt + die in
    if to_pt <= 24 then Some to_pt else None
;;

let enter_from_bar (p : Player_kind.t) ~(die : int) : int option =
  match p with
  | Player_kind.White ->
    let pt = 25 - die in
    if 19 <= pt && pt <= 24 then Some pt else None
  | Player_kind.Black ->
    let pt = die in
    if 1 <= pt && pt <= 6 then Some pt else None
;;

let implied_dest_if_legal
  ~(st : Game_state.t)
  ~(p : Player_kind.t)
  ~(source : Location.t)
  ~(die : int)
  : Location.t option
  =
  let move = { Move.from_ = source; die } in
  match Game_state.make_move st move with
  | Error _ -> None
  | Ok st' ->
    (match source with
     | Location.Bar ->
       (match enter_from_bar p ~die with
        | Some pt -> Some (Location.Point pt)
        | None -> None)
     | Location.Point from_pt ->
       (match destination_point p ~from_pt ~die with
        | Some pt -> Some (Location.Point pt)
        | None ->
          let old_off = off_count st p in
          let new_off = off_count st' p in
          if new_off > old_off then Some Location.Off else None)
     | Location.Off -> None)
;;

let valid_destinations_for_source ~(st : Game_state.t) ~(p : Player_kind.t) ~(dice_left : int list) ~(source : Location.t)
  : Location.t list
  =
  dice_left
  |> List.filter_map ~f:(fun die -> implied_dest_if_legal ~st ~p ~source ~die)
  |> List.dedup_and_sort ~compare:Location.compare
;;

let valid_sources ~(st : Game_state.t) ~(p : Player_kind.t) ~(dice_left : int list) : Location.t list =
  let sources =
    let bar =
      if bar_count st p > 0 then [ Location.Bar ] else []
    in
    let points =
      List.range 1 25
      |> List.filter_map ~f:(fun pt ->
        match st.board.(pt - 1) with
        | None -> None
        | Some s ->
          if Player_kind.equal s.owner p && s.count > 0 then Some (Location.Point pt) else None)
    in
    bar @ points
  in
  sources
  |> List.filter ~f:(fun src ->
    not (List.is_empty (valid_destinations_for_source ~st ~p ~dice_left ~source:src)))
;;

let choose_die_for_dest
  ~(st : Game_state.t)
  ~(p : Player_kind.t)
  ~(dice_left : int list)
  ~(source : Location.t)
  ~(dest : Location.t)
  : int option
  =
  List.find dice_left ~f:(fun die ->
    match implied_dest_if_legal ~st ~p ~source ~die with
    | None -> false
    | Some d -> Location.equal d dest)
;;

(* =============================================================================
   Rendering primitives
   ============================================================================= *)

let colors =
  let board_surface = "#d4b896" in
  let board_border  = "#6b4e2e" in
  let board_bar     = "#b4875d" in
  let point_light   = "#f1c27d" in
  let point_dark    = "#8b4a3a" in
  let selection     = "#3b82f6" in
  let valid_source  = "#22c55e" in
  let black_fill    = "#222222" in
  let black_stroke  = "#111111" in
  let white_fill    = "#f7f4ee" in
  let white_stroke  = "#333333" in
  board_surface, board_border, board_bar, point_light, point_dark,
  selection, valid_source, black_fill, black_stroke, white_fill, white_stroke
;;

let checker_node ~cx ~cy ~r ~(owner : Player_kind.t) =
  let (_bsurf, _bborder, _bbar, _pl, _pd, _sel, _vs, black_fill, black_stroke, white_fill, white_stroke) = colors in
  let fill, stroke, sw =
    match owner with
    | Player_kind.Black -> black_fill, black_stroke, "1"
    | Player_kind.White -> white_fill, white_stroke, "2"
  in
  svg "circle"
    ~attrs:
      [ attr "cx" (f cx)
      ; attr "cy" (f cy)
      ; attr "r" (f r)
      ; attr "fill" fill
      ; attr "stroke" stroke
      ; attr "stroke-width" sw
      ]
    []
;;

let stack_on_point_nodes ~x ~y ~dir ~w ~h ~owner ~count =
  let checker_r = (w /. 2.0) -. 4.0 in
  let checker_spacing = Float.min (checker_r *. 2.0) (h /. 6.0) in
  let visible = Int.min count 5 in
  let circles =
    List.init visible ~f:(fun idx ->
      let cy =
        match dir with
        | Down -> y +. checker_r +. 4.0 +. (Float.of_int idx *. checker_spacing)
        | Up   -> y +. h -. checker_r -. 4.0 -. (Float.of_int idx *. checker_spacing)
      in
      checker_node ~cx:(x +. (w /. 2.0)) ~cy ~r:checker_r ~owner)
  in
  let count_label =
    if count > 5 then
      let label_y =
        match dir with
        | Down -> y +. (4.0 *. checker_spacing) +. checker_r +. 16.0
        | Up   -> y +. h -. (4.0 *. checker_spacing) -. checker_r -. 8.0
      in
      [ svg "text"
          ~attrs:
            [ attr "x" (f (x +. (w /. 2.0)))
            ; attr "y" (f label_y)
            ; attr "text-anchor" "middle"
            ; attr "font-size" "12"
            ; attr "font-weight" "700"
            ; attr "fill" (match owner with Player_kind.Black -> "#ffffff" | Player_kind.White -> "#111111")
            ]
          [ Vdom.Node.text (Int.to_string count) ]
      ]
    else
      []
  in
  circles @ count_label
;;

let point_node ~point_num ~(stack : Game_state.point_stack option) ~x ~y ~dir ~w ~h
  ~is_selected ~is_valid_source ~is_valid_dest ~on_click =
  let (_bsurf, _bborder, _bbar, point_light, point_dark, selection, valid_source, _bf, _bs, _wf, _ws) = colors in
  let is_even = (point_num mod 2) = 0 in
  let fill = if is_even then point_light else point_dark in
  let stroke, stroke_w, dash =
    if is_selected then selection, "3", ""
    else if is_valid_dest then selection, "2", "4,4"
    else if is_valid_source then valid_source, "2", ""
    else "none", "0", ""
  in
  let clickable = is_selected || is_valid_source || is_valid_dest in
  let triangle =
    svg "polygon"
      ~attrs:
        ([ attr "points" (triangle_points ~x ~y ~w ~h ~dir)
         ; attr "fill" fill
         ; attr "stroke" stroke
         ; attr "stroke-width" stroke_w
         ]
         @ (if String.is_empty dash then [] else [ attr "stroke-dasharray" dash ])
         @ (if clickable then [ Vdom.Attr.on_click (fun _ -> on_click); attr "style" "cursor:pointer" ] else []))
      []
  in
  let stacks =
    match stack with
    | None -> []
    | Some { owner; count } ->
      stack_on_point_nodes ~x ~y ~dir ~w ~h ~owner ~count
  in
  svg "g" ~attrs:[] (triangle :: stacks)
;;

let bar_node ~p ~count ~x ~y ~w ~h ~is_selected ~is_valid_source ~on_click =
  if count <= 0 then Vdom.Node.none else
  let (_bsurf, _bborder, _bbar, _pl, _pd, selection, valid_source, _bf, _bs, _wf, _ws) = colors in
  let checker_r = (w /. 2.0) -. 6.0 in
  let checker_spacing = (checker_r *. 2.0) +. 4.0 in
  let stroke, sw =
    if is_selected then selection, "3"
    else if is_valid_source then valid_source, "2"
    else "none", "0"
  in
  let clickable = is_selected || is_valid_source in
  let highlight =
    if is_selected || is_valid_source then
      [ svg "rect"
          ~attrs:
            ([ attr "x" (f (x +. 2.0))
             ; attr "y" (f (y +. 2.0))
             ; attr "width" (f (w -. 4.0))
             ; attr "height" (f (h -. 4.0))
             ; attr "fill" "none"
             ; attr "stroke" stroke
             ; attr "stroke-width" sw
             ; attr "rx" "2"
             ]
             @ (if clickable then [ Vdom.Attr.on_click (fun _ -> on_click); attr "style" "cursor:pointer" ] else []))
          []
      ]
    else []
  in
  let visible = Int.min count 4 in
  let circles =
    List.init visible ~f:(fun idx ->
      checker_node
        ~cx:(x +. (w /. 2.0))
        ~cy:(y +. checker_r +. 8.0 +. (Float.of_int idx *. checker_spacing))
        ~r:checker_r
        ~owner:p)
  in
  let label =
    if count > 4 then
      [ svg "text"
          ~attrs:
            [ attr "x" (f (x +. (w /. 2.0)))
            ; attr "y" (f (y +. h -. 8.0))
            ; attr "text-anchor" "middle"
            ; attr "font-size" "11"
            ; attr "font-weight" "700"
            ; attr "fill" (match p with Player_kind.Black -> "#ffffff" | Player_kind.White -> "#111111")
            ]
          [ Vdom.Node.text (Int.to_string count) ]
      ]
    else []
  in
  svg "g" ~attrs:[] (highlight @ circles @ label)
;;

let bear_off_node ~p ~count ~x ~y ~w ~h ~is_valid_dest ~on_click =
  let (board_surface, board_border, _bbar, _pl, _pd, selection, _vs, black_fill, black_stroke, white_fill, white_stroke) = colors in
  let stroke = if is_valid_dest then selection else board_border in
  let sw = if is_valid_dest then "2" else "1" in
  let dash = if is_valid_dest then "4,4" else "" in
  let clickable = is_valid_dest in
  let container =
    svg "rect"
      ~attrs:
        ([ attr "x" (f x)
         ; attr "y" (f y)
         ; attr "width" (f w)
         ; attr "height" (f h)
         ; attr "fill" board_surface
         ; attr "stroke" stroke
         ; attr "stroke-width" sw
         ; attr "rx" "2"
         ]
         @ (if String.is_empty dash then [] else [ attr "stroke-dasharray" dash ])
         @ (if clickable then [ Vdom.Attr.on_click (fun _ -> on_click); attr "style" "cursor:pointer" ] else []))
      []
  in
  let checker_h = 8.0 in
  let spacing = 2.0 in
  let fill, stroke2 =
    match p with
    | Player_kind.Black -> black_fill, black_stroke
    | Player_kind.White -> white_fill, white_stroke
  in
  let visible = Int.min count 12 in
  let stacks =
    List.init visible ~f:(fun idx ->
      let yy = y +. h -. (Float.of_int (idx + 1) *. (checker_h +. spacing)) in
      svg "rect"
        ~attrs:
          [ attr "x" (f (x +. 3.0))
          ; attr "y" (f yy)
          ; attr "width" (f (w -. 6.0))
          ; attr "height" (f checker_h)
          ; attr "fill" fill
          ; attr "stroke" stroke2
          ; attr "stroke-width" "1"
          ; attr "rx" "1"
          ]
        [])
  in
  let label =
    if count > 0 then
      [ svg "text"
          ~attrs:
            [ attr "x" (f (x +. (w /. 2.0)))
            ; attr "y" (f (y +. 14.0))
            ; attr "text-anchor" "middle"
            ; attr "font-size" "10"
            ; attr "font-weight" "700"
            ; attr "fill" "#111111"
            ]
          [ Vdom.Node.text (Int.to_string count) ]
      ]
    else []
  in
  svg "g" ~attrs:[] (container :: stacks @ label)
;;

(* =============================================================================
   SVG renderer
   ============================================================================= *)

let render_svg
  ~(st : Game_state.t)
  ~(p : Player_kind.t option)
  ~(selected : Location.t option)
  ~(valid_srcs : Location.t list)
  ~(valid_dests : Location.t list)
  ~(on_click_point : int -> unit Vdom.Effect.t)
  ~(on_click_bar : unit -> unit Vdom.Effect.t)
  ~(on_click_bearoff : Player_kind.t -> unit Vdom.Effect.t)
  =
  let (board_surface, board_border, board_bar, _pl, _pd, _sel, _vs, _bf, _bs, _wf, _ws) = colors in

  let bg =
    svg "rect"
      ~attrs:
        [ attr "x" (f board_x)
        ; attr "y" (f board_y)
        ; attr "width" (f board_width)
        ; attr "height" (f board_height)
        ; attr "fill" board_surface
        ; attr "stroke" board_border
        ; attr "stroke-width" "2"
        ; attr "rx" "4"
        ]
      []
  in

  let bar_bg =
    svg "rect"
      ~attrs:
        [ attr "x" (f bar_x)
        ; attr "y" (f board_y)
        ; attr "width" (f bar_w)
        ; attr "height" (f board_height)
        ; attr "fill" board_bar
        ]
      []
  in

  let is_valid_source_loc loc = List.mem valid_srcs loc ~equal:Location.equal in
  let is_valid_dest_loc loc = List.mem valid_dests loc ~equal:Location.equal in
  let selected_is loc = Option.value_map selected ~default:false ~f:(Location.equal loc) in

  let points_top =
    top_points
    |> List.mapi ~f:(fun idx pt ->
      let x = point_x idx in
      let y = top_y in
      let stack = st.board.(pt - 1) in
      point_node
        ~point_num:pt
        ~stack
        ~x
        ~y
        ~dir:Down
        ~w:point_width
        ~h:half_h
        ~is_selected:(selected_is (Location.Point pt))
        ~is_valid_source:(is_valid_source_loc (Location.Point pt))
        ~is_valid_dest:(is_valid_dest_loc (Location.Point pt))
        ~on_click:(on_click_point pt))
  in

  let points_bottom =
    bottom_points
    |> List.mapi ~f:(fun idx pt ->
      let x = point_x idx in
      let y = bottom_y in
      let stack = st.board.(pt - 1) in
      point_node
        ~point_num:pt
        ~stack
        ~x
        ~y
        ~dir:Up
        ~w:point_width
        ~h:half_h
        ~is_selected:(selected_is (Location.Point pt))
        ~is_valid_source:(is_valid_source_loc (Location.Point pt))
        ~is_valid_dest:(is_valid_dest_loc (Location.Point pt))
        ~on_click:(on_click_point pt))
  in

  let bar_top =
    bar_node
      ~p:Player_kind.Black
      ~count:st.bar_black
      ~x:bar_x
      ~y:top_y
      ~w:bar_w
      ~h:half_h
      ~is_selected:(selected_is Location.Bar && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
      ~is_valid_source:(is_valid_source_loc Location.Bar && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
      ~on_click:(on_click_bar ())
  in

  let bar_bottom =
    bar_node
      ~p:Player_kind.White
      ~count:st.bar_white
      ~x:bar_x
      ~y:bottom_y
      ~w:bar_w
      ~h:half_h
      ~is_selected:(selected_is Location.Bar && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
      ~is_valid_source:(is_valid_source_loc Location.Bar && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
      ~on_click:(on_click_bar ())
  in

  let bearoff_x = board_width +. 50.0 in
  let bearoff_w = 30.0 in

  let bearoff_bottom =
    bear_off_node
      ~p:Player_kind.Black
      ~count:st.off_black
      ~x:bearoff_x
      ~y:bottom_y
      ~w:bearoff_w
      ~h:half_h
      ~is_valid_dest:(is_valid_dest_loc Location.Off && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
      ~on_click:(on_click_bearoff Player_kind.Black)
  in

  let bearoff_top =
    bear_off_node
      ~p:Player_kind.White
      ~count:st.off_white
      ~x:bearoff_x
      ~y:top_y
      ~w:bearoff_w
      ~h:half_h
      ~is_valid_dest:(is_valid_dest_loc Location.Off && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
      ~on_click:(on_click_bearoff Player_kind.White)
  in

  let label_node ~x ~y ~txt =
    svg "text"
      ~attrs:
        [ attr "x" (f x)
        ; attr "y" (f y)
        ; attr "text-anchor" "middle"
        ; attr "font-size" "10"
        ; attr "fill" "#000000aa"
        ]
      [ Vdom.Node.text txt ]
  in

  let labels_top =
    top_points
    |> List.mapi ~f:(fun idx pt ->
      label_node ~x:(point_x idx +. (point_width /. 2.0)) ~y:8.0 ~txt:(Int.to_string pt))
  in

  let labels_bottom =
    bottom_points
    |> List.mapi ~f:(fun idx pt ->
      label_node ~x:(point_x idx +. (point_width /. 2.0)) ~y:(board_height +. 18.0) ~txt:(Int.to_string pt))
  in

  (* 关键：这里用 100%/100% 配合 .board-wrap 的 padding-bottom 产生高度 *)
  svg "svg"
    ~attrs:
      [ attr "viewBox" (sprintf "0 0 %s %s" (f svg_w) (f svg_h))
      ; attr "preserveAspectRatio" "xMidYMid meet"
      ; attr "width" "100%"
      ; attr "height" "100%"
      ; Vdom.Attr.class_ "board"
      ]
    (bg
     :: bar_bg
     :: points_top
     @ points_bottom
     @ [ bar_top; bar_bottom; bearoff_top; bearoff_bottom ]
     @ labels_top
     @ labels_bottom)
;;

(* =============================================================================
   HTML helpers
   ============================================================================= *)

let dice_view (dice_left : int list) =
  let die_box n =
    let style =
      String.concat
        ~sep:";"
        [ "width:40px"
        ; "height:40px"
        ; "display:flex"
        ; "align-items:center"
        ; "justify-content:center"
        ; "border:2px solid #fff"
        ; "border-radius:8px"
        ; "font-size:18px"
        ; "font-weight:800"
        ; "background:#111"
        ]
    in
    Vdom.Node.div ~attrs:[ attr "style" style ] [ Vdom.Node.text (Int.to_string n) ]
  in
  Vdom.Node.div
    ~attrs:[ attr "style" "display:flex;gap:10px;justify-content:center;align-items:center;padding:10px 0;" ]
    (List.map dice_left ~f:die_box)
;;

let status_text ~(st : Game_state.t) ~(selected : Location.t option) =
  match st.decision with
  | Decision.Winner w ->
    sprintf "%s wins!" (match w with Player_kind.White -> "White" | Player_kind.Black -> "Black")
  | Decision.In_progress { whose_turn; dice_left } ->
    let turn = match whose_turn with Player_kind.White -> "White" | Player_kind.Black -> "Black" in
    let phase =
      if List.is_empty dice_left then "Roll dice"
      else (
        match selected with
        | None -> "Select a highlighted piece"
        | Some _ -> "Select destination (or Cancel)")
    in
    sprintf "%s — %s" turn phase
;;

let small_btn ~label ~disabled ~on_click =
  Vdom.Node.button
    ~attrs:
      ([ Vdom.Attr.class_ "nav-btn"
       ; Vdom.Attr.on_click (fun _ -> on_click)
       ]
       @ if disabled then [ attr "disabled" "true" ] else [])
    [ Vdom.Node.text label ]
;;

(* =============================================================================
   Main app
   ============================================================================= *)

let app_component =
  let initial_state =
    match Game_state.create () with
    | Ok st ->
      { st with decision = Decision.In_progress { whose_turn = Player_kind.White; dice_left = [] } }
    | Error _ -> failwith "Failed to create initial game state"
  in

  let%sub st, set_st = Bonsai.state ~default_model:initial_state (module Game_state) in
  let%sub selected, set_selected = Bonsai.state ~default_model:None (module Selected_source) in

  let%arr st = st
  and set_st = set_st
  and selected = selected
  and set_selected = set_selected in

  let p_opt, dice_left = whose_turn_and_dice st in
  let ph = phase_of ~st ~selected in

  let valid_srcs =
    match p_opt with
    | None -> []
    | Some p -> valid_sources ~st ~p ~dice_left
  in

  let valid_dests =
    match p_opt, selected with
    | Some p, Some src -> valid_destinations_for_source ~st ~p ~dice_left ~source:src
    | _ -> []
  in

  let has_any_move = not (List.is_empty valid_srcs) in

  let do_new_game =
    match Game_state.create () with
    | Error _ -> Vdom.Effect.Ignore
    | Ok st0 ->
      let st1 = { st0 with decision = Decision.In_progress { whose_turn = Player_kind.White; dice_left = [] } } in
      Vdom.Effect.Many [ set_st st1; set_selected None ]
  in

  let do_roll =
    match st.decision with
    | Decision.Winner _ -> Vdom.Effect.Ignore
    | Decision.In_progress { whose_turn; dice_left = dl } ->
      if not (List.is_empty dl) then Vdom.Effect.Ignore
      else
        let dice = roll_dice_list () in
        let st' = { st with decision = Decision.In_progress { whose_turn; dice_left = dice } } in
        Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let do_cancel = set_selected None in

  let do_end_turn =
    match st.decision with
    | Decision.Winner _ -> Vdom.Effect.Ignore
    | Decision.In_progress { whose_turn; dice_left = dl } ->
      if List.is_empty dl then Vdom.Effect.Ignore
      else
        let st' =
          { st with decision = Decision.In_progress { whose_turn = Player_kind.opposite whose_turn; dice_left = [] } }
        in
        Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let handle_click_source (loc : Location.t) =
    match ph, p_opt with
    | Select_source, Some _
    | Select_destination, Some _ ->
      if List.mem valid_srcs loc ~equal:Location.equal then set_selected (Some loc) else Vdom.Effect.Ignore
    | _ -> Vdom.Effect.Ignore
  in

  let apply_move ~(source : Location.t) ~(dest : Location.t) =
    match p_opt with
    | None -> Vdom.Effect.Ignore
    | Some p ->
      (match choose_die_for_dest ~st ~p ~dice_left ~source ~dest with
       | None -> Vdom.Effect.Ignore
       | Some die ->
         let move = { Move.from_ = source; die } in
         (match Game_state.make_move st move with
          | Error _ -> Vdom.Effect.Ignore
          | Ok st' -> Vdom.Effect.Many [ set_st st'; set_selected None ]))
  in

  let handle_click_point (pt : int) =
    let loc = Location.Point pt in
    match ph, selected with
    | Select_source, _ -> handle_click_source loc
    | Select_destination, Some src ->
      if List.mem valid_dests loc ~equal:Location.equal
      then apply_move ~source:src ~dest:loc
      else handle_click_source loc
    | _ -> Vdom.Effect.Ignore
  in

  let handle_click_bar () =
    match ph with
    | Select_source
    | Select_destination -> handle_click_source Location.Bar
    | _ -> Vdom.Effect.Ignore
  in

  let handle_click_bearoff (_p : Player_kind.t) =
    match ph, selected with
    | Select_destination, Some src ->
      if List.mem valid_dests Location.Off ~equal:Location.equal
      then apply_move ~source:src ~dest:Location.Off
      else Vdom.Effect.Ignore
    | _ -> Vdom.Effect.Ignore
  in

  let title =
    Vdom.Node.div
      ~attrs:[ attr "style" "text-align:center;font-size:24px;font-weight:800;margin-top:6px;margin-bottom:8px;" ]
      [ Vdom.Node.text "Backgammon" ]
  in

  let status_bar =
    let text = status_text ~st ~selected in
    Vdom.Node.div
      ~attrs:
        [ attr "style"
            "display:flex;justify-content:space-between;gap:12px;align-items:center;\
             background:#333;border:1px solid #555;border-radius:10px;padding:10px 12px;margin-bottom:10px;"
        ]
      [ Vdom.Node.div ~attrs:[ attr "style" "font-weight:700;font-size:14px;" ] [ Vdom.Node.text text ]
      ; Vdom.Node.div ~attrs:[ attr "style" "font-size:12px;color:#ccc;" ]
          [ Vdom.Node.text (sprintf "Off: B %d / W %d" st.off_black st.off_white) ]
      ]
  in

  let board_svg =
    render_svg
      ~st
      ~p:p_opt
      ~selected
      ~valid_srcs
      ~valid_dests
      ~on_click_point:handle_click_point
      ~on_click_bar:handle_click_bar
      ~on_click_bearoff:handle_click_bearoff
  in

  let buttons =
    let roll_disabled =
      match ph with
      | Roll_dice -> false
      | _ -> true
    in
    let end_turn_disabled =
      match ph with
      | Select_source
      | Select_destination -> List.is_empty dice_left
      | _ -> true
    in
    let cancel_disabled =
      match ph with
      | Select_destination -> false
      | _ -> true
    in
    let hint_txt =
      match ph with
      | Roll_dice -> "Click Roll"
      | Select_source -> if has_any_move then "Tap a green source" else "No moves — End Turn"
      | Select_destination -> "Tap a blue destination (or Cancel)"
      | Winner -> "New Game?"
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "topbar" ]
      [ small_btn ~label:"New Game" ~disabled:false ~on_click:do_new_game
      ; small_btn ~label:"Roll" ~disabled:roll_disabled ~on_click:do_roll
      ; small_btn ~label:"End Turn" ~disabled:end_turn_disabled ~on_click:do_end_turn
      ; small_btn ~label:"Cancel" ~disabled:cancel_disabled ~on_click:do_cancel
      ; Vdom.Node.div ~attrs:[ attr "style" "text-align:center;color:#bbb;font-size:13px;padding:6px 0;" ]
          [ Vdom.Node.text hint_txt ]
      ]
  in

  (* 关键：用你 CSS 设计的 .game + .board-wrap 结构 *)
  let game_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "board-wrap" ]
          [ board_svg ]
      ]
  in

  let dice_row = if List.is_empty dice_left then Vdom.Node.none else dice_view dice_left in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "page" ]
    [ title
    ; status_bar
    ; game_area
    ; dice_row
    ; buttons
    ]
;;

let () =
  Random.self_init ();
  Bonsai_web.Start.start app_component
;;
