open! Core
open! Hw2_backgammon_logic

let ok_exn r = Result.ok r |> Option.value_exn

let bar_count (t : Game_state.t) (p : Player_kind.t) =
  match p with
  | White -> t.bar_white
  | Black -> t.bar_black
;;

let terminal_score (state : Game_state.t) : int =
  match state.decision with
  | Decision.Winner pk ->
    (match pk with
     | White -> Int.max_value
     | Black -> Int.min_value)
  | In_progress _ -> 0
;;

let whose_turn (state : Game_state.t) : Player_kind.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | In_progress { whose_turn; dice_left = _ } -> Some whose_turn
;;


let points_with_owner (state : Game_state.t) ~(owner : Player_kind.t) : int list =
  let acc = ref [] in
  Array.iteri state.board ~f:(fun i cell ->
    match cell with
    | None -> ()
    | Some s ->
      if Player_kind.equal s.owner owner then acc := (i + 1) :: !acc);
  List.rev !acc
;;

let get_all_moves (state : Game_state.t) : Move.t list =
  match state.decision with
  | Decision.Winner _ -> []
  | In_progress { whose_turn; dice_left = dice } ->
    let dice = List.filter dice ~f:(fun d -> 1 <= d && d <= 6) in
    if List.is_empty dice
    then []
    else (
      let froms : Location.t list =
        if bar_count state whose_turn > 0
        then [ Location.Bar ]
        else points_with_owner state ~owner:whose_turn |> List.map ~f:(fun i -> Location.Point i)
      in
      List.concat_map froms ~f:(fun from_ ->
        List.map dice ~f:(fun die -> { Move.from_; die })))
;;

let legal_moves (state : Game_state.t) : Move.t list =
  get_all_moves state
  |> List.filter ~f:(fun m -> Game_state.make_move state m |> Result.is_ok)
;;

let random_move (state : Game_state.t) : Move.t option =
  match state.decision with
  | Decision.Winner _ -> None
  | In_progress _ ->
    let moves = legal_moves state in
    List.random_element moves
;;

let heuristic_value (state : Game_state.t) : int =
  (* 先用最保守的：终局极值，非终局 0。
     “更强”主要靠 rollout（模拟对局）来区分。 *)
  match state.decision with
  | Decision.Winner _ -> terminal_score state
  | In_progress _ -> 0
;;

let rollout ~(rng : Random.State.t) ~(max_plies : int) (state0 : Game_state.t) : int =
  let rec loop state ply =
    if ply <= 0
    then heuristic_value state
    else (
      match state.decision with
      | Decision.Winner _ -> terminal_score state
      | In_progress _ ->
        let moves = legal_moves state in
        match moves with
        | [] -> heuristic_value state
        | _ ->
          let m = List.nth_exn moves (Random.State.int rng (List.length moves)) in
          let next = Game_state.make_move state m |> ok_exn in
          loop next (ply - 1))
  in
  loop state0 max_plies
;;

let better_move
      (state : Game_state.t)
      ~(time_limit : Time_ns.Span.t)
      ~(random_seed : int)
  : Move.t option
  =
  match state.decision with
  | Decision.Winner _ -> None
  | In_progress { whose_turn = turn; dice_left = _ } ->
    let moves_and_children =
      legal_moves state
      |> List.filter_map ~f:(fun m ->
        Game_state.make_move state m |> Result.ok |> Option.map ~f:(fun child -> m, child))
    in
    if List.is_empty moves_and_children
    then None
        else (
      let rng = Random.State.make [| random_seed |] in

      let max_player = Player_kind.White in
      let maximizing_now = Player_kind.equal turn max_player in

      let module Move_key = struct
        type t = Move.t [@@deriving sexp, compare]
      end
      in
      let tbl : (Move_key.t, int * int) Hashtbl.t = Hashtbl.Poly.create () in
      List.iter moves_and_children ~f:(fun (m, _c) -> Hashtbl.set tbl ~key:m ~data:(0, 0));

      let max_plies = 2000 in

      let deadline = Time_ns.add (Time_ns.now ()) time_limit in
      let before_deadline () = Time_ns.compare (Time_ns.now ()) deadline < 0 in

      while before_deadline () do
        List.iter moves_and_children ~f:(fun (m, child) ->
          if before_deadline () then (
            let score = rollout ~rng ~max_plies child in
            let sum, cnt = Hashtbl.find_exn tbl m in
            Hashtbl.set tbl ~key:m ~data:(sum + score, cnt + 1)
          ))
      done;

      let avg_score m =
        let sum, cnt = Hashtbl.find_exn tbl m in
        if cnt = 0 then 0 else sum / cnt
      in
      let compare_move m1 m2 = Int.compare (avg_score m1) (avg_score m2) in

      let all_moves = List.map moves_and_children ~f:fst in
      if maximizing_now
      then List.max_elt all_moves ~compare:compare_move
      else List.min_elt all_moves ~compare:compare_move)
;;


let play_games ~n ~random_seed make_initial player_a player_b =
  let _rng = Random.State.make [| random_seed |] in
  let a_wins = ref 0 in
  let b_wins = ref 0 in
  let draws = ref 0 in

  (* 约定：A = White, B = Black（和 terminal_score 的方向一致）。 *)
  let record_winner = function
    | Player_kind.White -> incr a_wins
    | Player_kind.Black -> incr b_wins
  in

  let rec play_one (state : Game_state.t) =
    match state.decision with
    | Decision.Winner pk -> record_winner pk
    | In_progress _ ->
      let move_opt =
        match whose_turn state with
        | None -> None
        | Some Player_kind.White -> player_a state
        | Some Player_kind.Black -> player_b state
      in
      (match move_opt with
       | None -> incr draws
       | Some m ->
         (match Game_state.make_move state m |> Result.ok with
          | None -> incr draws
          | Some next -> play_one next))
  in

  for _i = 1 to n do
    play_one (make_initial ())
  done;

  !a_wins, !b_wins, !draws
;;
