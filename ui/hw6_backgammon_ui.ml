open! Core
open Virtual_dom
open! Bonsai.Let_syntax

open Backgammon_logic_library
open Hw2_backgammon_logic

(* =============================================================================
   Small helpers
   ============================================================================= *)

let attr k v = Vdom.Attr.create k v
let testid s = Vdom.Attr.create "data-testid" s
let f (x : float) : string = Printf.sprintf "%g" x
let svg tag ~attrs children = Vdom.Node.create_svg tag ~attrs children

let clamp_die n = if n < 1 then 1 else if n > 6 then 6 else n

let parse_dice_csv (s : string) : int list =
  (* accepts: "3,4" "3 4" "3, 4,6" *)
  s
  |> String.map ~f:(fun c -> if Char.equal c ',' then ' ' else c)
  |> String.split ~on:' '
  |> List.filter ~f:(fun x -> not (String.is_empty (String.strip x)))
  |> List.filter_map ~f:(fun tok ->
    match Int.of_string_opt (String.strip tok) with
    | None -> None
    | Some n ->
      if 1 <= n && n <= 6 then Some n else None)

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

let player_to_string = function
  | Player_kind.White -> "white"
  | Player_kind.Black -> "black"
;;

(* =============================================================================
   UI model
   ============================================================================= *)

module Selected_source = struct
  type t = Location.t option [@@deriving sexp, compare, equal]
end

module Debug_open = struct
  type t = bool [@@deriving sexp, compare, equal]
end

module Debug_dice = struct
  type t = string [@@deriving sexp, compare, equal]
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
      ; attr "pointer-events" "none" 
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
            ; attr "pointer-events" "none"
            ]
          [ Vdom.Node.text (Int.to_string count) ]
      ]
    else
      []
  in
  circles @ count_label
;;

let point_node
  ~point_num
  ~(stack : Game_state.point_stack option)
  ~x ~y ~dir ~w ~h
  ~is_selected ~is_valid_source ~is_valid_dest
  ~on_click
  =
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
         ; attr "data-testid" (sprintf "point-%d" point_num)
         ; attr "data-point" (Int.to_string point_num)
         ; attr "role" (if clickable then "button" else "img")
         ; attr "aria-label" (sprintf "point %d" point_num)
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
  let first_checker_ring =
    match stack with
    | Some { owner = _; count } when count > 0 && (is_valid_source || is_selected) ->
      let visible = Int.min count 5 in
      let idx = Int.max 0 (visible - 1) in
      let checker_r = (w /. 2.0) -. 4.0 in
      let checker_spacing = Float.min (checker_r *. 2.0) (h /. 6.0) in

      let cy =
        match dir with
        | Down -> y +. checker_r +. 4.0 +. (Float.of_int idx *. checker_spacing)
        | Up   -> y +. h -. checker_r -. 4.0 -. (Float.of_int idx *. checker_spacing)
      in

      let ring = "#60a5fa" in

      [ 
        svg "circle"
          ~attrs:
            [ attr "cx" (f (x +. (w /. 2.0)))
            ; attr "cy" (f cy)
            ; attr "r"  (f (checker_r +. 5.5))
            ; attr "fill" "none"
            ; attr "stroke" "rgba(96,165,250,0.35)"
            ; attr "stroke-width" "6"
            ; attr "style" "pointer-events:none"
            ]
          []
      ; 
        svg "circle"
          ~attrs:
            [ attr "cx" (f (x +. (w /. 2.0)))
            ; attr "cy" (f cy)
            ; attr "r"  (f (checker_r +. 2.6))
            ; attr "fill" "none"
            ; attr "stroke" ring
            ; attr "stroke-width" "3"
            ; attr "style"
                ("pointer-events:none;"
                ^ "filter: drop-shadow(0 0 6px " ^ ring ^ ") "
                ^ "drop-shadow(0 0 12px rgba(96,165,250,0.8));")
            ]
          []
      ]
    | _ -> []
  in
  svg "g" ~attrs:[] (triangle :: (stacks @ first_checker_ring))
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
  let testid = sprintf "bar-%s" (player_to_string p) in
  let highlight =
    if is_selected || is_valid_source then
      [ svg "rect"
          ~attrs:
            ([ attr "x" (f (x +. 2.0))
             ; attr "y" (f (y +. 2.0))
             ; attr "width" (f (w -. 4.0))
             ; attr "height" (f (h -. 4.0))
             ; attr "fill" "transparent"
             ; attr "style" "cursor:pointer;pointer-events:all"
             ; attr "stroke" stroke
             ; attr "stroke-width" sw
             ; attr "rx" "2"
             ; attr "data-testid" testid
             ; attr "role" "button"
             ; attr "aria-label" testid
             ]
             @ (if clickable then [ Vdom.Attr.on_click (fun _ -> on_click); attr "style" "cursor:pointer" ] else []))
          []
      ]
    else
      [ svg "rect"
          ~attrs:
            [ attr "x" (f (x +. 2.0))
            ; attr "y" (f (y +. 2.0))
            ; attr "width" (f (w -. 4.0))
            ; attr "height" (f (h -. 4.0))
            ; attr "fill" "transparent"
            ; attr "stroke" "transparent"
            ; attr "data-testid" testid
            ; attr "role" "img"
            ; attr "aria-label" testid
            ]
          []
      ]
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
  let testid = sprintf "off-%s" (player_to_string p) in
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
         ; attr "data-testid" testid
         ; attr "role" (if clickable then "button" else "img")
         ; attr "aria-label" testid
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
        ; attr "data-testid" "board-bg"
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
        ; attr "data-testid" "board-bar"
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

  svg "svg"
    ~attrs:
      [ attr "viewBox" (sprintf "0 0 %s %s" (f svg_w) (f svg_h))
      ; attr "preserveAspectRatio" "xMidYMid meet"
      ; attr "width" "100%"
      ; attr "height" "100%"
      ; Vdom.Attr.class_ "board"
      ; attr "data-testid" "board-svg"
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
  let pip_offsets n =
    (* returns list of (dx,dy) in {-1,0,1} grid coords *)
    match n with
    | 1 -> [ 0, 0 ]
    | 2 -> [ -1, -1; 1, 1 ]
    | 3 -> [ -1, -1; 0, 0; 1, 1 ]
    | 4 -> [ -1, -1; 1, -1; -1, 1; 1, 1 ]
    | 5 -> [ -1, -1; 1, -1; 0, 0; -1, 1; 1, 1 ]
    | 6 -> [ -1, -1; -1, 0; -1, 1; 1, -1; 1, 0; 1, 1 ]
    | _ -> [ 0, 0 ]
  in
  let die_svg idx n =
    let size = 40.0 in
    let r = 8.0 in
    let pip_r = 3.2 in
    let cx0 = size /. 2.0 in
    let cy0 = size /. 2.0 in
    let step = 10.0 in
    let pips =
      pip_offsets n
      |> List.map ~f:(fun (dx, dy) ->
        svg "circle"
          ~attrs:
            [ attr "cx" (f (cx0 +. (Float.of_int dx *. step)))
            ; attr "cy" (f (cy0 +. (Float.of_int dy *. step)))
            ; attr "r" (f pip_r)
            ; attr "fill" "#fff"
            ]
          [])
    in
    svg "svg"
      ~attrs:
        [ attr "width" "40"
        ; attr "height" "40"
        ; attr "viewBox" "0 0 40 40"
        ; attr "data-testid" (sprintf "die-%d" idx)
        ]
      (svg "rect"
         ~attrs:
           [ attr "x" "1.5"
           ; attr "y" "1.5"
           ; attr "width" "37"
           ; attr "height" "37"
           ; attr "rx" (f r)
           ; attr "fill" "#111"
           ; attr "stroke" "#fff"
           ; attr "stroke-width" "2"
           ]
         []
       :: pips)
  in
  Vdom.Node.div
    ~attrs:
      [ attr "style"
          "display:flex;gap:10px;justify-content:center;align-items:center;padding:10px 0;"
      ; attr "data-testid" "dice-row"
      ]
    (List.mapi dice_left ~f:(fun idx n -> die_svg idx n))
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

let small_btn ~testid ~label ~disabled ~on_click =
  Vdom.Node.button
    ~attrs:
      ([ Vdom.Attr.class_ "nav-btn"
       ; attr "data-testid" testid
       ; attr "aria-label" label
       ]
       @ (if disabled then [ attr "disabled" "true" ] else [ Vdom.Attr.on_click (fun _ -> on_click) ]))
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
  let%sub debug_open, set_debug_open = Bonsai.state ~default_model:false (module Debug_open) in
  let%sub debug_dice, set_debug_dice = Bonsai.state ~default_model:"6,6" (module Debug_dice) in

  let%arr st = st
  and set_st = set_st
  and selected = selected
  and set_selected = set_selected
  and debug_open = debug_open
  and set_debug_open = set_debug_open
  and debug_dice = debug_dice
  and set_debug_dice = set_debug_dice in

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

  (* Debug: force dice to a stable value for tests *)
  let do_apply_debug_dice =
    match st.decision with
    | Decision.Winner _ -> Vdom.Effect.Ignore
    | Decision.In_progress { whose_turn; _ } ->
      let dice = parse_dice_csv debug_dice in
      if List.is_empty dice then Vdom.Effect.Ignore
      else
        let st' = { st with decision = Decision.In_progress { whose_turn; dice_left = dice } } in
        Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let do_toggle_debug = set_debug_open (not debug_open) in

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
         let move = { Move.from_ = source; die = clamp_die die } in
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
      ~attrs:[ attr "style" "text-align:center;font-size:24px;font-weight:800;margin-top:6px;margin-bottom:8px;"; attr "data-testid" "title" ]
      [ Vdom.Node.text "Backgammon" ]
  in

  let status_bar =
    let text = status_text ~st ~selected in
    let turn_txt =
      match p_opt with
      | None -> "none"
      | Some p -> player_to_string p
    in
    Vdom.Node.div
      ~attrs:
        [ testid "status"
        ; attr "style"
            "display:flex;justify-content:space-between;gap:12px;align-items:center;\
             background:#333;border:1px solid #555;border-radius:10px;padding:10px 12px;margin-bottom:10px;"
        ; attr "data-testid" "status-bar"
        ]
      [ Vdom.Node.div
          ~attrs:[ attr "style" "font-weight:700;font-size:14px;"; attr "data-testid" "status-text" ]
          [ Vdom.Node.text text ]
      ; Vdom.Node.div
          ~attrs:[ attr "style" "font-size:12px;color:#ccc;"; attr "data-testid" "status-meta" ]
          [ Vdom.Node.text (sprintf "turn=%s | Off: B %d / W %d" turn_txt st.off_black st.off_white) ]
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
      ~attrs:[ Vdom.Attr.class_ "topbar"; attr "data-testid" "controls" ]
      [ small_btn ~testid:"btn-new" ~label:"New Game" ~disabled:false ~on_click:do_new_game
      ; small_btn ~testid:"btn-roll" ~label:"Roll" ~disabled:roll_disabled ~on_click:do_roll
      ; small_btn ~testid:"btn-end" ~label:"End Turn" ~disabled:end_turn_disabled ~on_click:do_end_turn
      ; small_btn ~testid:"btn-cancel" ~label:"Cancel" ~disabled:cancel_disabled ~on_click:do_cancel
      ; small_btn ~testid:"btn-debug" ~label:"Debug" ~disabled:false ~on_click:do_toggle_debug
      ; Vdom.Node.div ~attrs:[ attr "style" "text-align:center;color:#bbb;font-size:13px;padding:6px 0;"; attr "data-testid" "hint" ]
          [ Vdom.Node.text hint_txt ]
      ]
  in

  let debug_panel =
    if not debug_open then Vdom.Node.none
    else
      Vdom.Node.div
        ~attrs:
          [ attr "data-testid" "debug-panel"
          ; attr "style"
              "margin-top:10px;padding:10px;border:1px dashed #666;border-radius:10px;background:#222;color:#ddd;"
          ]
        [ Vdom.Node.div
            ~attrs:[ attr "style" "font-weight:800;margin-bottom:6px;" ]
            [ Vdom.Node.text "Debug (for tests)" ]
        ; Vdom.Node.div
            ~attrs:[ attr "style" "display:flex;gap:8px;align-items:center;flex-wrap:wrap;" ]
            [ Vdom.Node.label
                ~attrs:[ attr "style" "font-size:12px;color:#bbb;"; attr "for" "debug-dice" ]
                [ Vdom.Node.text "Force dice (e.g. 6,6 or 3,4):" ]
            ; Vdom.Node.input
                ~attrs:
                  [ attr "id" "debug-dice"
                  ; attr "data-testid" "debug-dice-input"
                  ; attr "value" debug_dice
                  ; attr "style" "padding:6px 8px;border-radius:8px;border:1px solid #555;background:#111;color:#eee;"
                  ; Vdom.Attr.on_input (fun _ s -> set_debug_dice s)
                  ]
                ()
            ; Vdom.Node.button
                ~attrs:
                  [ attr "data-testid" "debug-apply"
                  ; attr "style" "padding:6px 10px;border-radius:8px;border:1px solid #777;background:#111;color:#eee;cursor:pointer;"
                  ; Vdom.Attr.on_click (fun _ -> do_apply_debug_dice)
                  ]
                [ Vdom.Node.text "Apply Dice" ]
            ]
        ]
  in

  let game_area =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game"; attr "data-testid" "game" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "board-wrap"; attr "data-testid" "board-wrap" ]
          [ board_svg ]
      ]
  in

  let dice_row = if List.is_empty dice_left then Vdom.Node.none else dice_view dice_left in

  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "page"; attr "data-testid" "page" ]
    [ title
    ; status_bar
    ; game_area
    ; dice_row
    ; debug_panel
    ; buttons
    ]
;;

let () =
  Random.self_init ();
  Bonsai_web.Start.start app_component
;;
