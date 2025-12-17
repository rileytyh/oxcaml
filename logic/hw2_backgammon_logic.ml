open! Core

module Player_kind = struct
  type t =
    | White
    | Black
  [@@deriving sexp, compare, equal]

  let opposite = function
    | White -> Black
    | Black -> White
  ;;
end

module Location = struct
  type t =
    | Point of int
    | Bar
    | Off
  [@@deriving sexp, compare, equal]
end

module Move = struct
  type t =
    { from_ : Location.t
    ; die : int
    }
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t; dice_left : int list }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  let is_game_over = function
    | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type point_stack =
    { owner : Player_kind.t
    ; count : int
    }
  [@@deriving sexp, compare, equal]

  type t =
    { board : point_stack option array
    ; bar_white : int
    ; bar_black : int
    ; off_white : int
    ; off_black : int
    ; decision : Decision.t
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Bad_board_size
    [@@deriving sexp, compare, equal]
  end

  let initial_board () : point_stack option array =
    [|
      Some { owner = Player_kind.Black; count = 2 };
      None; None; None; None;
      Some { owner = Player_kind.White; count = 5 };
      None;
      Some { owner = Player_kind.White; count = 3 };
      None; None; None;
      Some { owner = Player_kind.Black; count = 5 };
      Some { owner = Player_kind.White; count = 5 };
      None; None; None;
      Some { owner = Player_kind.Black; count = 3 };
      None;
      Some { owner = Player_kind.Black; count = 5 };
      None; None; None; None;
      Some { owner = Player_kind.White; count = 2 };
    |]

  let create () : (t, Create_error.t) Result.t =
    let board = initial_board () in
    if Array.length board <> 24
    then Error Bad_board_size
    else
      Ok
        { board
        ; bar_white = 0
        ; bar_black = 0
        ; off_white = 0
        ; off_black = 0
        ; decision = Decision.In_progress { whose_turn = White; dice_left = [ 6; 1 ] }
        }
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Wrong_turn
      | Die_not_available
      | Illegal_from
      | Illegal_to
      | Must_enter_from_bar
      | Blocked
      | Cannot_bear_off
      | Internal_error
    [@@deriving sexp, compare, equal]
  end

  let bar_count t (p : Player_kind.t) =
    match p with
    | White -> t.bar_white
    | Black -> t.bar_black
  ;;

  let set_bar_count t (p : Player_kind.t) n =
    match p with
    | White -> { t with bar_white = n }
    | Black -> { t with bar_black = n }
  ;;

  let off_count t (p : Player_kind.t) =
    match p with
    | White -> t.off_white
    | Black -> t.off_black
  ;;

  let set_off_count t (p : Player_kind.t) n =
    match p with
    | White -> { t with off_white = n }
    | Black -> { t with off_black = n }
  ;;

  let point_index_exn (n : int) : int =
    if n < 1 || n > 24 then failwith "point out of range";
    n - 1
  ;;

  let _has_any_checker_on_board (t : t) (p : Player_kind.t) : bool =
    Array.exists t.board ~f:(function
      | None -> false
      | Some s -> Player_kind.equal s.owner p)
  ;;

  let in_home (p : Player_kind.t) (pt : int) : bool =
    match p with
    | White -> 1 <= pt && pt <= 6
    | Black -> 19 <= pt && pt <= 24
  ;;

  let all_in_home (t : t) (p : Player_kind.t) : bool =
    let on_board_ok =
      Array.for_alli t.board ~f:(fun i cell ->
        match cell with
        | None -> true
        | Some s ->
          if Player_kind.equal s.owner p
          then in_home p (i + 1)
          else true)
    in
    on_board_ok && bar_count t p = 0
  ;;

  let destination_point (p : Player_kind.t) ~(from_pt : int) ~(die : int) : int option =
    match p with
    | White ->
      let to_pt = from_pt - die in
      if to_pt >= 1 then Some to_pt else None
    | Black ->
      let to_pt = from_pt + die in
      if to_pt <= 24 then Some to_pt else None
  ;;

  let enter_from_bar (p : Player_kind.t) ~(die : int) : int option =
    match p with
    | White ->
      let pt = 25 - die in
      if 19 <= pt && pt <= 24 then Some pt else None
    | Black ->
      let pt = die in
      if 1 <= pt && pt <= 6 then Some pt else None
  ;;

  let remove_one_die (dice_left : int list) (die : int) : int list option =
    let rec go acc = function
      | [] -> None
      | x :: xs ->
        if x = die then Some (List.rev_append acc xs) else go (x :: acc) xs
    in
    go [] dice_left
  ;;

  let stack_inc (s : point_stack) = { s with count = s.count + 1 }
  let stack_dec (s : point_stack) = { s with count = s.count - 1 }

  let set_stack (board : point_stack option array) (pt : int) (v : point_stack option) =
    board.(point_index_exn pt) <- v
  ;;

  let blocked_by_opponent ~(dest : point_stack option) ~(p : Player_kind.t) : bool =
    match dest with
    | None -> false
    | Some s -> (not (Player_kind.equal s.owner p)) && s.count >= 2
  ;;

  let apply_land
        ~(t : t)
        ~(p : Player_kind.t)
        ~(dest_pt : int)
        ~(board : point_stack option array)
    : (t * point_stack option array, Move_error.t) Result.t
    =
    let dest = board.(point_index_exn dest_pt) in
    if blocked_by_opponent ~dest ~p
    then Error Move_error.Blocked
    else (
      match dest with
      | None ->
        set_stack board dest_pt (Some { owner = p; count = 1 });
        Ok (t, board)
      | Some s when Player_kind.equal s.owner p ->
        set_stack board dest_pt (Some (stack_inc s));
        Ok (t, board)
      | Some s ->
        if s.count = 1
        then (
          set_stack board dest_pt (Some { owner = p; count = 1 });
          let opp = Player_kind.opposite p in
          let t' = set_bar_count t opp (bar_count t opp + 1) in
          Ok (t', board))
        else Error Move_error.Blocked)
  ;;

  let remove_from_point
        ~(t : t)
        ~(p : Player_kind.t)
        ~(from_pt : int)
        ~(board : point_stack option array)
    : (t * point_stack option array, Move_error.t) Result.t
    =
    match board.(point_index_exn from_pt) with
    | None -> Error Move_error.Illegal_from
    | Some s ->
      if not (Player_kind.equal s.owner p) then Error Move_error.Illegal_from
      else if s.count <= 0 then Error Move_error.Internal_error
      else (
        if s.count = 1 then set_stack board from_pt None else set_stack board from_pt (Some (stack_dec s));
        Ok (t, board))
  ;;

  let decide_next ~(t : t) ~(p : Player_kind.t) ~(dice_left : int list) : Decision.t =
    if off_count t p >= 15
    then Decision.Winner p
    else if List.is_empty dice_left
    then Decision.In_progress { whose_turn = Player_kind.opposite p; dice_left = [] }
    else Decision.In_progress { whose_turn = p; dice_left }
  ;;

  let make_move (t : t) ({ from_; die } : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Decision.Winner _ -> Error Move_error.Game_is_over
    | Decision.In_progress { whose_turn; dice_left } ->
      let p = whose_turn in
      if die <= 0 || die > 6 then Error Move_error.Illegal_to
      else (
        match remove_one_die dice_left die with
        | None -> Error Move_error.Die_not_available
        | Some dice_left' ->
          let must_enter = bar_count t p > 0 in
          let board = Array.copy t.board in
          let t0 = t in
          let%bind.Result t1, board1 =
            match from_ with
            | Location.Bar ->
              if not must_enter then Error Move_error.Illegal_from
              else (
                match enter_from_bar p ~die with
                | None -> Error Move_error.Illegal_to
                | Some dest_pt ->
                  let t_bar = set_bar_count t0 p (bar_count t0 p - 1) in
                  apply_land ~t:t_bar ~p ~dest_pt ~board)
            | Location.Point from_pt ->
              if must_enter then Error Move_error.Must_enter_from_bar
              else if from_pt < 1 || from_pt > 24 then Error Move_error.Illegal_from
              else (
                match destination_point p ~from_pt ~die with
                | Some dest_pt ->
                  let%bind.Result t2, board2 = remove_from_point ~t:t0 ~p ~from_pt ~board in
                  apply_land ~t:t2 ~p ~dest_pt ~board:board2
                | None ->
                  if all_in_home t0 p && in_home p from_pt
                  then (
                    let%bind.Result t2, board2 = remove_from_point ~t:t0 ~p ~from_pt ~board in
                    let t3 = set_off_count t2 p (off_count t2 p + 1) in
                    Ok (t3, board2))
                  else Error Move_error.Cannot_bear_off)
            | Location.Off -> Error Move_error.Illegal_from
          in
          let decision = decide_next ~t:t1 ~p ~dice_left:dice_left' in
          Ok { t1 with board = board1; decision })
  ;;
end
