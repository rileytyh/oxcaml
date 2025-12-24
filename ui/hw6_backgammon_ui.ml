(* ui/hw6_backgammon_ui.ml *)

open! Core
open Virtual_dom
open! Bonsai.Let_syntax
open Js_of_ocaml

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
    | Some n -> if 1 <= n && n <= 6 then Some n else None)

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

let short_id (s : string) : string =
  let s = String.strip s in
  if String.length s <= 12 then s else String.prefix s 6 ^ "…" ^ String.suffix s 2
;;

(* =============================================================================
   JS bridge: OCaml <-> Firebase/Quickmatch/Invite JS
   ============================================================================= *)

module Js_bridge = struct
  (* Keep a cached snapshot of the latest OCaml state as S-expression. *)
  let latest_state_sexp : string ref = ref ""

  let set_latest_state (sexp : string) =
    latest_state_sexp := sexp

  (* Safely read window.firebaseEnv.<field> as string option *)
  let get_firebase_string_field (field : string) : string option =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let v = Js.Unsafe.get env field in
      match Js.to_string (Js.typeof v) with
      | "string" -> Some (Js.to_string v)
      | _ -> None
    with
    | _ -> None

  (* Safely read window.firebaseEnv.<field> as bool option *)
  let get_firebase_bool_field (field : string) : bool option =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let v = Js.Unsafe.get env field in
      match Js.to_string (Js.typeof v) with
      | "boolean" -> Some (Js.to_bool v)
      | "string" ->
        let s = String.strip (String.lowercase (Js.to_string v)) in
        if String.equal s "true" || String.equal s "1" || String.equal s "yes" then Some true
        else if String.equal s "false" || String.equal s "0" || String.equal s "no" then Some false
        else None
      | _ -> None
    with
    | _ -> None

  let get_firebase_room_has_state () : bool option =
    get_firebase_bool_field "roomHasState"

  (* Read role set by quickmatch.js: window.firebaseEnv.role = "white" | "black" *)
  let get_firebase_role () : string option =
    get_firebase_string_field "role"

  (* uid / room / status *)
  let get_firebase_uid () : string option =
    get_firebase_string_field "uid"

  let get_firebase_room_id () : string option =
    get_firebase_string_field "roomId"

  let get_firebase_status () : string option =
    get_firebase_string_field "status"

  (* Call a zero-arg JS function: window.firebaseEnv.<name>() *)
  let call_env0 (name : string) : unit =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let f = Js.Unsafe.get env name in
      let ty = Js.to_string (Js.typeof f) in
      if String.equal ty "function"
      then ignore (Js.Unsafe.fun_call f [||])
      else ()
    with
    | _ -> ()

  (* Call a one-arg JS function: window.firebaseEnv.<name>(string) *)
  let call_env1 (name : string) (arg : string) : unit =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let f = Js.Unsafe.get env name in
      let ty = Js.to_string (Js.typeof f) in
      if String.equal ty "function"
      then ignore (Js.Unsafe.fun_call f [| Js.Unsafe.inject (Js.string arg) |])
      else ()
    with
    | _ -> ()

  (* Send the given S-expression to JS (Firebase) if possible. *)
  let send_state (sexp : string) : unit =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let f = Js.Unsafe.get env "sendState" in
      ignore (Js.Unsafe.fun_call f [| Js.Unsafe.inject (Js.string sexp) |])
    with
    | _ -> ()

  (* Copy helper (sync) — avoid navigator.clipboard Promise rejection issues. *)
  let copy_to_clipboard (s : string) : unit =
    try
      let doc = Dom_html.document in
      let ta = Dom_html.createTextarea doc in
      Js.Unsafe.set ta "value" (Js.string s);
      let style = Js.Unsafe.get ta "style" in
      Js.Unsafe.set style "position" (Js.string "fixed");
      Js.Unsafe.set style "left" (Js.string "-1000px");
      Js.Unsafe.set style "top" (Js.string "-1000px");
      Js.Unsafe.set style "opacity" (Js.string "0");

      let body = Js.Unsafe.get doc "body" in
      ignore (Js.Unsafe.meth_call body "appendChild" [| Js.Unsafe.inject ta |]);

      (* select + copy must happen in the same user gesture *)
      ignore (Js.Unsafe.meth_call ta "focus" [||]);
      ignore (Js.Unsafe.meth_call ta "select" [||]);
      ignore (Js.Unsafe.meth_call doc "execCommand" [| Js.Unsafe.inject (Js.string "copy") |]);

      ignore (Js.Unsafe.meth_call body "removeChild" [| Js.Unsafe.inject ta |])
    with
    | _ -> ()

  (* Read firebaseEnv.incomingInvites (array of objects with fields: id, fromUid, roomId) *)
  type invite =
    { id : string
    ; from_uid : string option
    ; room_id : string option
    }

  let get_incoming_invites () : invite list =
    try
      let env = Js.Unsafe.get Dom_html.window "firebaseEnv" in
      let arr = Js.Unsafe.get env "incomingInvites" in
      let len_v = Js.Unsafe.get arr "length" in
      let len =
        match Js.to_string (Js.typeof len_v) with
        | "number" -> int_of_float (Js.float_of_number len_v)
        | _ -> 0
      in
      List.init len ~f:(fun i ->
        let it = Js.Unsafe.get arr i in
        let get_str (k : string) =
          try
            let v = Js.Unsafe.get it k in
            if String.equal (Js.to_string (Js.typeof v)) "string"
            then Some (Js.to_string v)
            else None
          with _ -> None
        in
        let id = Option.value (get_str "id") ~default:"" in
        { id; from_uid = get_str "fromUid"; room_id = get_str "roomId" })
      |> List.filter ~f:(fun inv -> not (String.is_empty inv.id))
    with
    | _ -> []

  (* Expose OCaml entrypoints for JS:
     - set_state(sexpString): apply remote state coming from Firestore
     - request_send(): ask OCaml to re-send its current state
  *)
  let expose_ocaml_remote
      ~(apply_remote : string -> unit)
      ~(request_send : unit -> unit)
    =
    let remote = Js.Unsafe.obj [||] in
    Js.Unsafe.set remote "set_state"
      (Js.wrap_callback (fun (s : Js.js_string Js.t) ->
         apply_remote (Js.to_string s)));
    Js.Unsafe.set remote "request_send"
      (Js.wrap_callback (fun () -> request_send ()));
    Js.Unsafe.set Dom_html.window "ocamlRemote" remote
end

(* =============================================================================
   Debug visibility via URL: ?debug=1
   ============================================================================= *)

let url_debug_enabled () : bool =
  (* Accept: ?debug=1 / ?debug=true / ?debug=yes / ?debug *)
  let location = Js.Unsafe.get Dom_html.window "location" in
  let search = Js.to_string (Js.Unsafe.get location "search") in
  let s =
    match String.chop_prefix search ~prefix:"?" with
    | Some x -> x
    | None -> search
  in
  if String.is_empty s then false
  else
    let parts = String.split s ~on:'&' in
    let is_truthy = function
      | None -> true
      | Some v ->
        let v = String.strip v in
        String.Caseless.equal v "1"
        || String.Caseless.equal v "true"
        || String.Caseless.equal v "yes"
        || String.Caseless.equal v "on"
    in
    List.exists parts ~f:(fun kv ->
      match String.lsplit2 kv ~on:'=' with
      | None ->
        (* e.g. "?debug" *)
        String.Caseless.equal (String.strip kv) "debug"
      | Some (k, v) ->
        let k = String.strip k in
        if String.Caseless.equal k "debug" then is_truthy (Some v) else false)

(* =============================================================================
   UI model
   ============================================================================= *)

module Selected_source = struct
  type t = Location.t option [@@deriving sexp, compare, equal]
end

module Debug_open = struct
  type t = bool [@@deriving sexp, compare, equal]
end

module Advanced_open = struct
  type t = bool [@@deriving sexp, compare, equal]
end

module Debug_dice = struct
  type t = string [@@deriving sexp, compare, equal]
end

module Last_sent = struct
  type t = string option [@@deriving sexp, compare, equal]
end

module Opt_string = struct
  type t = string option [@@deriving sexp, compare, equal]
end

module Opt_bool = struct
  type t = bool option [@@deriving sexp, compare, equal]
end

module Seen_remote = struct
  type t = bool [@@deriving sexp, compare, equal]
end

module Applying_remote = struct
  type t = bool [@@deriving sexp, compare, equal]
end

module Text = struct
  type t = string [@@deriving sexp, compare, equal]
end

(* (A) payload module *)
module Push_payload = struct
  type t =
    { sexp : string
    ; should_send : bool
    }
  [@@deriving sexp, compare, equal]
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

let valid_destinations_for_source
  ~(st : Game_state.t)
  ~(p : Player_kind.t)
  ~(dice_left : int list)
  ~(source : Location.t)
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
  let tid = sprintf "bar-%s" (player_to_string p) in
  let highlight =
    [ svg "rect"
        ~attrs:
          ([ attr "x" (f (x +. 2.0))
           ; attr "y" (f (y +. 2.0))
           ; attr "width" (f (w -. 4.0))
           ; attr "height" (f (h -. 4.0))
           ; attr "fill" "transparent"
           ; attr "stroke" stroke
           ; attr "stroke-width" sw
           ; attr "rx" "2"
           ; attr "data-testid" tid
           ; attr "role" (if clickable then "button" else "img")
           ; attr "aria-label" tid
           ]
           @ (if clickable then [ Vdom.Attr.on_click (fun _ -> on_click); attr "style" "cursor:pointer" ] else []))
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
  let tid = sprintf "off-%s" (player_to_string p) in
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
         ; attr "data-testid" tid
         ; attr "role" (if clickable then "button" else "img")
         ; attr "aria-label" tid
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
  let (board_surface, board_border, board_bar, point_light, point_dark, selection, valid_source, _black_fill, _black_stroke, _white_fill, _white_stroke) = colors in

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

  let point_node
    ~point_num
    ~(stack : Game_state.point_stack option)
    ~x ~y ~dir ~w ~h
    ~is_selected ~is_valid_source ~is_valid_dest
    ~on_click
    =
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
    svg "g" ~attrs:[] (triangle :: stacks)
  in

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
      ~is_selected:
        (selected_is Location.Bar
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
      ~is_valid_source:
        (is_valid_source_loc Location.Bar
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
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
      ~is_selected:
        (selected_is Location.Bar
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
      ~is_valid_source:
        (is_valid_source_loc Location.Bar
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
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
      ~is_valid_dest:
        (is_valid_dest_loc Location.Off
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.Black))
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
      ~is_valid_dest:
        (is_valid_dest_loc Location.Off
         && Option.value_map p ~default:false ~f:(Player_kind.equal Player_kind.White))
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
      label_node
        ~x:(point_x idx +. (point_width /. 2.0))
        ~y:(board_height +. 18.0)
        ~txt:(Int.to_string pt))
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
    let r = 8.0 in
    let pip_r = 3.2 in
    let cx0 = 20.0 in
    let cy0 = 20.0 in
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
    sprintf
      "%s wins!"
      (match w with
       | Player_kind.White -> "White"
       | Player_kind.Black -> "Black")
  | Decision.In_progress { whose_turn; dice_left } ->
    let turn =
      match whose_turn with
      | Player_kind.White -> "White"
      | Player_kind.Black -> "Black"
    in
    let phase =
      if List.is_empty dice_left
      then "Roll dice"
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

let input_box ~id ~value ~placeholder ~on_input =
  Vdom.Node.input
    ~attrs:
      [ attr "id" id
      ; attr "data-testid" id
      ; attr "value" value
      ; attr "placeholder" placeholder
      ; attr "style"
          "width:280px;padding:10px 12px;border-radius:12px;border:1px solid #333;\
           background:#0b0b0b;color:#fff;outline:none;"
      ; Vdom.Attr.on_input (fun _ s -> on_input s)
      ]
    ()
;;

let lobby_btn ~id ~label ~disabled ~on_click =
  let base_style =
    "width:280px;padding:12px 14px;border-radius:14px;border:1px solid #333;\
     background:#111;color:#fff;font-weight:800;font-size:15px;cursor:pointer;\
     box-shadow:0 8px 18px rgba(0,0,0,0.25);"
  in
  let disabled_style =
    "width:280px;padding:12px 14px;border-radius:14px;border:1px solid #333;\
     background:#111;color:#fff;font-weight:800;font-size:15px;opacity:0.45;cursor:not-allowed;\
     box-shadow:0 8px 18px rgba(0,0,0,0.25);"
  in
  Vdom.Node.button
    ~attrs:
      ([ attr "data-testid" id
       ; attr "style" (if disabled then disabled_style else base_style)
       ]
       @ (if disabled
          then [ attr "disabled" "true" ]
          else [ Vdom.Attr.on_click (fun _ -> on_click) ]))
    [ Vdom.Node.text label ]
;;

let meta_line ~k ~v =
  Vdom.Node.div
    ~attrs:[ attr "style" "font-size:12px;color:#bbb;display:flex;gap:8px;justify-content:space-between;" ]
    [ Vdom.Node.span ~attrs:[ attr "style" "color:#888;" ] [ Vdom.Node.text k ]
    ; Vdom.Node.span ~attrs:[ attr "style" "color:#ddd;font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;" ]
        [ Vdom.Node.text v ]
    ]
;;

(* =============================================================================
   Main app
   ============================================================================= *)

let app_component =
  let debug_allowed = url_debug_enabled () in

  let initial_state =
    match Game_state.create () with
    | Ok st ->
      { st with decision = Decision.In_progress { whose_turn = Player_kind.White; dice_left = [] } }
    | Error _ -> failwith "Failed to create initial game state"
  in

  let%sub st, set_st = Bonsai.state ~default_model:initial_state (module Game_state) in
  let%sub selected, set_selected = Bonsai.state ~default_model:None (module Selected_source) in
  let%sub debug_open, set_debug_open = Bonsai.state ~default_model:false (module Debug_open) in
  let%sub advanced_open, set_advanced_open =
    Bonsai.state ~default_model:false (module Advanced_open)
  in
  let%sub debug_dice, set_debug_dice = Bonsai.state ~default_model:"6,6" (module Debug_dice) in
  let%sub last_sent, set_last_sent = Bonsai.state ~default_model:None (module Last_sent) in

  (* env state (so OCaml re-renders when JS changes firebaseEnv fields) *)
  let%sub env_uid, set_env_uid = Bonsai.state ~default_model:None (module Opt_string) in
  let%sub env_room, set_env_room = Bonsai.state ~default_model:None (module Opt_string) in
  let%sub env_status, set_env_status = Bonsai.state ~default_model:None (module Opt_string) in
  let%sub env_role, set_env_role = Bonsai.state ~default_model:None (module Opt_string) in
  let%sub env_room_has_state, set_env_room_has_state =
    Bonsai.state ~default_model:None (module Opt_bool)
  in

  (* invite UI inputs *)
  let%sub invite_to_uid, set_invite_to_uid = Bonsai.state ~default_model:"" (module Text) in
  let%sub join_room_text, set_join_room_text = Bonsai.state ~default_model:"" (module Text) in

  (* small toast message for UI feedback (e.g., copied) *)
  let%sub toast, set_toast = Bonsai.state ~default_model:None (module Opt_string) in

  (* have we applied at least one remote state? *)
  let%sub seen_remote, set_seen_remote =
    Bonsai.state ~default_model:false (module Seen_remote)
  in

  (* suppress echo-send when applying remote state *)
  let%sub applying_remote, set_applying_remote =
    Bonsai.state ~default_model:false (module Applying_remote)
  in

  (* Listen to window "firebaseEnvChanged" and copy fields into Bonsai state *)
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map set_env_uid = set_env_uid
         and set_env_room = set_env_room
         and set_env_status = set_env_status
         and set_env_role = set_env_role
         and set_env_room_has_state = set_env_room_has_state
         in
         let initial_eff =
           Vdom.Effect.Many
             [ set_env_uid (Js_bridge.get_firebase_uid ())
             ; set_env_room (Js_bridge.get_firebase_room_id ())
             ; set_env_status (Js_bridge.get_firebase_status ())
             ; set_env_role (Js_bridge.get_firebase_role ())
             ; set_env_room_has_state (Js_bridge.get_firebase_room_has_state ())
             ]
         in
         let attach () =
           let cb =
             Js.wrap_callback (fun (ev_any : Js.Unsafe.any) ->
               let ev : Dom_html.event Js.t = Obj.magic ev_any in
               let eff =
                 Vdom.Effect.Many
                   [ set_env_uid (Js_bridge.get_firebase_uid ())
                   ; set_env_room (Js_bridge.get_firebase_room_id ())
                   ; set_env_status (Js_bridge.get_firebase_status ())
                   ; set_env_role (Js_bridge.get_firebase_role ())
                   ; set_env_room_has_state (Js_bridge.get_firebase_room_has_state ())
                   ]
               in
               ignore (Bonsai_web.Effect.Expert.handle ev eff : unit))
           in
           ignore
             (Js.Unsafe.meth_call
                Dom_html.window
                "addEventListener"
                [| Js.Unsafe.inject (Js.string "firebaseEnvChanged")
                 ; Js.Unsafe.inject cb
                 ; Js.Unsafe.inject Js._false
                |]);
           Js.Unsafe.set Dom_html.window "__ocamlFirebaseCb" cb
         in
         Vdom.Effect.Many [ Vdom.Effect.of_sync_fun attach (); initial_eff ])
      ()
  in

   (* When signed in, ensure invite listener starts (in case JS didn't start it) *)
   let%sub () =
     Bonsai.Edge.on_change
       (module Opt_string)
       (let%map env_uid = env_uid in env_uid)
       ~callback:
         (Bonsai.Value.return (fun uid_opt ->
            match uid_opt with
            | None -> Vdom.Effect.Ignore
            | Some _ ->
              Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env0 "listenIncomingInvites") ()))
   in

  (* Expose window.ocamlRemote.set_state + request_send once on mount *)
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map set_st = set_st
         and set_selected = set_selected
         and set_last_sent = set_last_sent
         and set_seen_remote = set_seen_remote
         and set_applying_remote = set_applying_remote
         in
         let sync_fn () =
           Js_bridge.expose_ocaml_remote
             ~apply_remote:(fun sexp_str ->
                 try
                   let st_remote =
                     Game_state.t_of_sexp (Sexplib.Sexp.of_string sexp_str)
                   in
                   let eff =
                     Vdom.Effect.Many
                       [ set_applying_remote true
                       ; set_last_sent (Some sexp_str)
                       ; set_seen_remote true
                       ; set_st st_remote
                       ; set_selected None
                       ; set_applying_remote false
                       ]
                   in
                   let dummy_ev : #Dom_html.event Js.t =
                     Obj.magic (Js.Unsafe.obj [||])
                   in
                   (Bonsai_web.Effect.Expert.handle dummy_ev eff : unit)
                 with
                 | _ -> ())
             ~request_send:(fun () ->
                 let s = !(Js_bridge.latest_state_sexp) in
                 let role_opt = Js_bridge.get_firebase_role () in
                 let role_player : Player_kind.t option =
                   match role_opt with
                   | Some r when String.Caseless.equal (String.strip r) "white" -> Some Player_kind.White
                   | Some r when String.Caseless.equal (String.strip r) "black" -> Some Player_kind.Black
                   | _ -> None
                 in
                 let room_has_state =
                   match Js_bridge.get_firebase_room_has_state () with
                   | None -> true
                   | Some b -> b
                 in
                 if String.is_empty (String.strip s) then ()
                 else
                   match role_player with
                   | Some Player_kind.White when not room_has_state -> Js_bridge.send_state s
                   | _ -> ())
         in
         (Vdom.Effect.of_sync_fun sync_fn) ())
      ()
  in

  (* Whenever local state changes, push it to Firestore if allowed *)
  let%sub () =
    Bonsai.Edge.on_change
      (module Push_payload)
      (let%map st = st
       and env_role = env_role
       and env_status = env_status
       and env_uid = env_uid
       and env_room_has_state = env_room_has_state
       and seen_remote = seen_remote
       and applying_remote = applying_remote
       in
       let sexp_str = Sexplib.Sexp.to_string (Game_state.sexp_of_t st) in
       Js_bridge.set_latest_state sexp_str;

       let role_player : Player_kind.t option =
         match env_role with
         | Some r when String.Caseless.equal (String.strip r) "white" -> Some Player_kind.White
         | Some r when String.Caseless.equal (String.strip r) "black" -> Some Player_kind.Black
         | _ -> None
       in
       let matched =
         match env_status with
         | Some s -> String.Caseless.equal (String.strip s) "matched"
         | None -> false
       in

       let room_has_state =
         match env_room_has_state with
         | None -> true
         | Some b -> b
       in

       let should_send =
         Option.is_some env_uid
         && matched
         && Option.is_some role_player
         && (seen_remote || not room_has_state)
         && not applying_remote
       in
       { Push_payload.sexp = sexp_str; should_send })
      ~callback:
        (let%map last_sent = last_sent
         and set_last_sent = set_last_sent
         in
         fun ({ Push_payload.sexp; should_send } : Push_payload.t) ->
           if not should_send then Vdom.Effect.Ignore
           else
             match last_sent with
             | Some s when String.equal s sexp ->
               Vdom.Effect.Ignore
             | _ ->
               Js_bridge.send_state sexp;
               set_last_sent (Some sexp))
  in

  let%arr st = st
  and set_st = set_st
  and selected = selected
  and set_selected = set_selected
  and debug_open = debug_open
  and set_debug_open = set_debug_open
  and advanced_open = advanced_open
  and set_advanced_open = set_advanced_open
  and debug_dice = debug_dice
  and set_debug_dice = set_debug_dice
  and set_last_sent = set_last_sent
  and env_uid = env_uid
  and env_room = env_room
  and env_status = env_status
  and env_role = env_role
  and invite_to_uid = invite_to_uid
  and set_invite_to_uid = set_invite_to_uid
  and join_room_text = join_room_text
  and set_join_room_text = set_join_room_text
  and toast = toast
  and set_toast = set_toast
  in

  let p_opt, dice_left = whose_turn_and_dice st in
  let ph = phase_of ~st ~selected in

  let my_role_opt : Player_kind.t option =
    match env_role with
    | Some r ->
      let r = String.strip r in
      if String.Caseless.equal r "white" then Some Player_kind.White
      else if String.Caseless.equal r "black" then Some Player_kind.Black
      else None
    | None -> None
  in

  let uid_opt = env_uid in
  let room_opt = env_room in
  let is_signed_in = Option.is_some uid_opt in
  let is_matched =
    match env_status with
    | Some s -> String.Caseless.equal (String.strip s) "matched"
    | None -> false
  in

  let can_interact =
    match my_role_opt, p_opt with
    | Some mine, Some turn -> Player_kind.equal mine turn
    | _ -> false
  in

  let valid_srcs =
    if not can_interact
    then []
    else
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
    if not can_interact then Vdom.Effect.Ignore
    else
      match Game_state.create () with
      | Error _ -> Vdom.Effect.Ignore
      | Ok st0 ->
        let st1 =
          { st0 with decision = Decision.In_progress { whose_turn = Player_kind.White; dice_left = [] } }
        in
        let sexp = Sexplib.Sexp.to_string (Game_state.sexp_of_t st1) in
        Js_bridge.set_latest_state sexp;
        Js_bridge.send_state sexp;
        Vdom.Effect.Many
          [ set_last_sent (Some sexp)
          ; set_st st1
          ; set_selected None
          ]
  in

  let do_roll =
    if not can_interact then Vdom.Effect.Ignore
    else
      match st.decision with
      | Decision.Winner _ -> Vdom.Effect.Ignore
      | Decision.In_progress { whose_turn; dice_left = dl } ->
        if not (List.is_empty dl)
        then Vdom.Effect.Ignore
        else
          let dice = roll_dice_list () in
          let st' = { st with decision = Decision.In_progress { whose_turn; dice_left = dice } } in
          Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let do_cancel = if not can_interact then Vdom.Effect.Ignore else set_selected None in

  let do_end_turn =
    if not can_interact then Vdom.Effect.Ignore
    else
      match st.decision with
      | Decision.Winner _ -> Vdom.Effect.Ignore
      | Decision.In_progress { whose_turn; dice_left = dl } ->
        if List.is_empty dl
        then Vdom.Effect.Ignore
        else
          let st' =
            { st with
              decision =
                Decision.In_progress { whose_turn = Player_kind.opposite whose_turn; dice_left = [] }
            }
          in
          Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let do_apply_debug_dice =
    if not can_interact then Vdom.Effect.Ignore
    else
      match st.decision with
      | Decision.Winner _ -> Vdom.Effect.Ignore
      | Decision.In_progress { whose_turn; _ } ->
        let dice = parse_dice_csv debug_dice in
        if List.is_empty dice
        then Vdom.Effect.Ignore
        else
          let st' = { st with decision = Decision.In_progress { whose_turn; dice_left = dice } } in
          Vdom.Effect.Many [ set_st st'; set_selected None ]
  in

  let do_toggle_debug =
    if not debug_allowed then Vdom.Effect.Ignore else set_debug_open (not debug_open)
  in

  let handle_click_source (loc : Location.t) =
    if not can_interact then Vdom.Effect.Ignore
    else
      match ph, p_opt with
      | Select_source, Some _
      | Select_destination, Some _ ->
        if List.mem valid_srcs loc ~equal:Location.equal
        then set_selected (Some loc)
        else Vdom.Effect.Ignore
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
         match Game_state.make_move st move with
         | Error _ -> Vdom.Effect.Ignore
         | Ok st' ->
           let sexp = Sexplib.Sexp.to_string (Game_state.sexp_of_t st') in
           Js_bridge.set_latest_state sexp;
           Vdom.Effect.Many [ set_st st'; set_selected None ])
  in

  let handle_click_point (pt : int) =
    if not can_interact then Vdom.Effect.Ignore
    else
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
    if not can_interact then Vdom.Effect.Ignore
    else
      match ph with
      | Select_source
      | Select_destination -> handle_click_source Location.Bar
      | _ -> Vdom.Effect.Ignore
  in

  let handle_click_bearoff (_p : Player_kind.t) =
    if not can_interact then Vdom.Effect.Ignore
    else
      match ph, selected with
      | Select_destination, Some src ->
        if List.mem valid_dests Location.Off ~equal:Location.equal
        then apply_move ~source:src ~dest:Location.Off
        else Vdom.Effect.Ignore
      | _ -> Vdom.Effect.Ignore
  in

  let title =
    Vdom.Node.div
      ~attrs:
        [ attr "style"
            "text-align:center;font-size:24px;font-weight:800;margin-top:6px;margin-bottom:8px;"
        ; attr "data-testid" "title"
        ]
      [ Vdom.Node.text "Backgammon" ]
  in

  let pretty_role = function
    | None -> "—"
    | Some Player_kind.White -> "White"
    | Some Player_kind.Black -> "Black"
  in
  let pretty_turn = function
    | None -> "—"
    | Some Player_kind.White -> "White"
    | Some Player_kind.Black -> "Black"
  in

  let match_txt =
    match env_status with
    | Some s when String.Caseless.equal (String.strip s) "matched" -> "Matched ✅"
    | Some s when not (String.is_empty (String.strip s)) -> String.capitalize (String.strip s)
    | _ -> if is_matched then "Matched ✅" else "Not matched"
  in
  let uid_txt = Option.value_map uid_opt ~default:"—" ~f:short_id in
  let room_txt = Option.value_map room_opt ~default:"—" ~f:short_id in

  let status_bar =
    let text = status_text ~st ~selected in
    Vdom.Node.div
      ~attrs:
        [ testid "status"
        ; attr "style"
            "display:flex;justify-content:space-between;gap:12px;align-items:center;\
             background:#333;border:1px solid #555;border-radius:10px;padding:10px 12px;margin-bottom:10px;"
        ; attr "data-testid" "status-bar"
        ]
      [ Vdom.Node.div
          ~attrs:[ attr "style" "font-weight:800;font-size:14px;"; attr "data-testid" "status-text" ]
          [ Vdom.Node.text text ]
      ; Vdom.Node.div
          ~attrs:[ attr "style" "font-size:12px;color:#ddd;line-height:1.4;text-align:right;"; attr "data-testid" "status-meta" ]
          [ Vdom.Node.div
              ~attrs:[ attr "data-testid" "online-line-1" ]
              [ Vdom.Node.text
                  (sprintf "Online: %s  ·  You: %s  ·  Turn: %s"
                     match_txt
                     (pretty_role my_role_opt)
                     (pretty_turn p_opt))
              ]
          ; Vdom.Node.div
              ~attrs:[ attr "data-testid" "online-line-2"; attr "style" "color:#bbb;" ]
              [ Vdom.Node.text (sprintf "UID: %s  ·  Room: %s" uid_txt room_txt) ]
          ]
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
      | Roll_dice -> (not can_interact)
      | _ -> true
    in
    let end_turn_disabled =
      if not can_interact then true
      else
        match ph with
        | Select_source
        | Select_destination -> List.is_empty dice_left
        | _ -> true
    in
    let cancel_disabled =
      if not can_interact then true
      else
        match ph with
        | Select_destination -> false
        | _ -> true
    in
    let new_game_disabled = not can_interact in
    let hint_txt =
      if not can_interact then "Waiting for opponent…"
      else
        match ph with
        | Roll_dice -> "Click Roll"
        | Select_source -> if has_any_move then "Tap a green source" else "No moves — End Turn"
        | Select_destination -> "Tap a blue destination (or Cancel)"
        | Winner -> "New Game?"
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "topbar"; attr "data-testid" "controls" ]
      ([
         small_btn ~testid:"btn-new" ~label:"New Game" ~disabled:new_game_disabled ~on_click:do_new_game
       ; small_btn ~testid:"btn-roll" ~label:"Roll" ~disabled:roll_disabled ~on_click:do_roll
       ; small_btn ~testid:"btn-end" ~label:"End Turn" ~disabled:end_turn_disabled ~on_click:do_end_turn
       ; small_btn ~testid:"btn-cancel" ~label:"Cancel" ~disabled:cancel_disabled ~on_click:do_cancel
       ]
       @ (if debug_allowed
          then [ small_btn ~testid:"btn-debug" ~label:"Debug" ~disabled:false ~on_click:do_toggle_debug ]
          else [])
       @ [ Vdom.Node.div
             ~attrs:
               [ attr "style" "text-align:center;color:#bbb;font-size:13px;padding:6px 0;"
               ; attr "data-testid" "hint"
               ]
             [ Vdom.Node.text hint_txt ]
         ])
  in

  let debug_panel =
    if (not debug_allowed) || (not debug_open)
    then Vdom.Node.none
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
                  ; attr "style"
                      "padding:6px 8px;border-radius:8px;border:1px solid #555;background:#111;color:#eee;"
                  ; Vdom.Attr.on_input (fun _ s -> set_debug_dice s)
                  ]
                ()
            ; Vdom.Node.button
                ~attrs:
                  [ attr "data-testid" "debug-apply"
                  ; attr "style"
                      "padding:6px 10px;border-radius:8px;border:1px solid #777;background:#111;color:#eee;cursor:pointer;"
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

  let status_is_waiting =
    match env_status with
    | Some s -> String.Caseless.equal (String.strip s) "waiting"
    | None -> false
  in

  (* -------------------- LOBBY (with Invite + Copy/Join) -------------------- *)

  let incoming =
    Js_bridge.get_incoming_invites ()
  in

  let invites_view =
    if List.is_empty incoming then
      Vdom.Node.div
        ~attrs:[ attr "style" "color:#777;font-size:13px;text-align:center;margin-top:6px;" ]
        [ Vdom.Node.text "No incoming invites." ]
    else
      Vdom.Node.div
        ~attrs:[ attr "style" "display:flex;flex-direction:column;gap:10px;margin-top:6px;" ]
        (List.map incoming ~f:(fun inv ->
           let from_txt = Option.value_map inv.from_uid ~default:"—" ~f:short_id in
           let room_txt2 = Option.value_map inv.room_id ~default:"—" ~f:short_id in
           Vdom.Node.div
             ~attrs:[ attr "style" "border:1px solid #333;border-radius:14px;background:#0f0f0f;padding:10px 12px;" ]
             [ meta_line ~k:"From UID" ~v:from_txt
             ; meta_line ~k:"Room" ~v:room_txt2
             ; meta_line ~k:"Invite ID" ~v:(short_id inv.id)
             ; Vdom.Node.div
                 ~attrs:[ attr "style" "display:flex;gap:10px;justify-content:center;margin-top:10px;" ]
                 [ lobby_btn
                     ~id:(sprintf "btn-accept-%s" inv.id)
                     ~label:"Accept"
                     ~disabled:(not is_signed_in)
                     ~on_click:(Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env1 "acceptInvite" inv.id) ())
                 ; lobby_btn
                     ~id:(sprintf "btn-decline-%s" inv.id)
                     ~label:"Decline"
                     ~disabled:(not is_signed_in)
                     ~on_click:(Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env1 "declineInvite" inv.id) ())
                 ]
             ]))
  in

  let card ?(title=None) children =
    Vdom.Node.div
      ~attrs:[
        attr "style"
          "width:320px;border:1px solid #333;border-radius:16px;background:#0d0d0d;padding:12px 14px;"
      ]
      (match title with
       | None -> children
       | Some t ->
         Vdom.Node.div ~attrs:[ attr "style" "font-weight:900;margin-bottom:8px;" ]
           [ Vdom.Node.text t ]
         :: children)
  in

  let lobby_card =
    Vdom.Node.div
      ~attrs:
        [ attr "data-testid" "lobby"
        ; attr "style"
            "min-height:80vh;display:flex;flex-direction:column;align-items:center;\
             justify-content:center;gap:12px;padding:16px;"
        ]
      ([ Vdom.Node.div
           ~attrs:[ attr "style" "font-size:44px;font-weight:900;margin-bottom:6px;" ]
           [ Vdom.Node.text "Backgammon" ]
       ; (match toast with
          | None -> Vdom.Node.none
          | Some t ->
            Vdom.Node.div
              ~attrs:[ attr "style" "margin-top:-2px;margin-bottom:6px;color:#9fe870;font-weight:900;text-align:center;" ]
              [ Vdom.Node.text t ])
       ]
       @
       [ (* Status + Copy *)
         card
           [ meta_line ~k:"Status" ~v:match_txt
           ; meta_line ~k:"Your UID" ~v:uid_txt
           ; meta_line ~k:"Room" ~v:room_txt
           ; Vdom.Node.div
               ~attrs:[ attr "style" "display:flex;gap:10px;justify-content:center;margin-top:10px;" ]
               [ lobby_btn
                   ~id:"btn-copy-uid"
                   ~label:"Copy UID"
                   ~disabled:(not is_signed_in)
                   ~on_click:(Vdom.Effect.Many
                     [ set_toast (Some "Copied UID ✓")
                     ; Vdom.Effect.of_sync_fun (fun () ->
                         match uid_opt with
                         | None -> ()
                         | Some u -> Js_bridge.copy_to_clipboard u) ()
                     ])
               ; lobby_btn
                   ~id:"btn-copy-room"
                   ~label:"Copy Room"
                   ~disabled:(not (Option.is_some room_opt))
                   ~on_click:(Vdom.Effect.Many
                     [ set_toast (Some "Copied Room ✓")
                     ; Vdom.Effect.of_sync_fun (fun () ->
                         match room_opt with
                         | None -> ()
                         | Some r -> Js_bridge.copy_to_clipboard r) ()
                     ])
               ]
           ]

       ; (* Primary actions *)
         lobby_btn
           ~id:"btn-signin"
           ~label:"Sign in (Guest)"
           ~disabled:is_signed_in
           ~on_click:(Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env0 "signIn") ())

       ; lobby_btn
           ~id:"btn-quickmatch"
           ~label:(if status_is_waiting then "Searching…" else "Quickmatch")
           ~disabled:((not is_signed_in) || status_is_waiting)
           ~on_click:(Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env0 "quickmatch") ())

       ; (* Invite *)
         card ~title:(Some "Invite a Friend")
           [ input_box
               ~id:"invite-to-uid-input"
               ~value:invite_to_uid
               ~placeholder:"Paste friend's UID…"
               ~on_input:set_invite_to_uid
           ; Vdom.Node.div ~attrs:[ attr "style" "height:8px;" ] []
           ; lobby_btn
               ~id:"btn-send-invite"
               ~label:"Send Invite"
               ~disabled:((not is_signed_in) || String.is_empty (String.strip invite_to_uid))
               ~on_click:(Vdom.Effect.of_sync_fun (fun () ->
                 Js_bridge.call_env1 "createInvite" (String.strip invite_to_uid)) ())
           ; Vdom.Node.div
               ~attrs:[ attr "style" "margin-top:10px;color:#777;font-size:12px;line-height:1.4;" ]
               [ Vdom.Node.text "Tip: open 2 browsers → both Sign in → copy UID → Send Invite → Accept." ]
           ]

       ; (* Incoming Invites *)
         card ~title:(Some "Incoming Invites")
           [ invites_view ]

       ; (* Advanced toggle *)
         lobby_btn
           ~id:"btn-advanced"
           ~label:(if advanced_open then "Advanced ▲" else "Advanced ▼")
           ~disabled:false
           ~on_click:(set_advanced_open (not advanced_open))

       ; (* Advanced content: Create/Join Room *)
         (if not advanced_open then Vdom.Node.none else
            card ~title:(Some "Room (Share / Join)")
              [ lobby_btn
                  ~id:"btn-create-room"
                  ~label:"Create Room (share code)"
                  ~disabled:(not is_signed_in)
                  ~on_click:(Vdom.Effect.of_sync_fun (fun () -> Js_bridge.call_env0 "createGame") ())
              ; Vdom.Node.div ~attrs:[ attr "style" "height:8px;" ] []
              ; input_box
                  ~id:"join-room-input"
                  ~value:join_room_text
                  ~placeholder:"Paste roomId to join…"
                  ~on_input:set_join_room_text
              ; Vdom.Node.div ~attrs:[ attr "style" "height:8px;" ] []
              ; lobby_btn
                  ~id:"btn-join-room"
                  ~label:"Join Room"
                  ~disabled:((not is_signed_in) || String.is_empty (String.strip join_room_text))
                  ~on_click:(Vdom.Effect.of_sync_fun (fun () ->
                    Js_bridge.call_env1 "joinGame" (String.strip join_room_text)) ())
              ])
       ])
  in

  let game_page =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "page"; attr "data-testid" "page" ]
      [ title
      ; status_bar
      ; game_area
      ; dice_row
      ; debug_panel
      ; buttons
      ]
  in

  if is_matched then game_page else lobby_card
;;

let () =
  Random.self_init ();
  Bonsai_web.Start.start app_component
;;
