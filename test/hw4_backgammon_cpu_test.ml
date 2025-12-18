open! Core
open! Backgammon_logic_library
open! Hw2_backgammon_logic
open! Hw4_backgammon_cpu

let ok_exn r = Result.ok r |> Option.value_exn

let%test "random_move returns legal move (if any)" =
  let state = Game_state.create () |> ok_exn in
  match random_move state with
  | None -> true
  | Some m -> Game_state.make_move state m |> Result.is_ok
;;

let%test "better_move returns legal move (if any)" =
  let state = Game_state.create () |> ok_exn in
  match better_move state ~time_limit:(Time_ns.Span.of_ms 30.) ~random_seed:1 with
  | None -> true
  | Some m -> Game_state.make_move state m |> Result.is_ok
;;

let%test "smoke: run some games without crashing" =
  let make_initial () = Game_state.create () |> ok_exn in
  let a state = random_move state in
  let b state = better_move state ~time_limit:(Time_ns.Span.of_ms 10.) ~random_seed:42 in
  let aw, bw, dr = play_games ~n:50 ~random_seed:123 make_initial a b in
  aw + bw + dr = 50
;;

