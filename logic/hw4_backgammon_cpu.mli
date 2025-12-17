open! Core
open! Hw2_backgammon_logic

val get_all_moves : Game_state.t -> Move.t list

val random_move : Game_state.t -> Move.t option

val better_move
  :  Game_state.t
  -> time_limit:Time_ns.Span.t
  -> random_seed:int
  -> Move.t option

val play_games
  :  n:int
  -> random_seed:int
  -> (unit -> Game_state.t)
  -> (Game_state.t -> Move.t option)  (* player A *)
  -> (Game_state.t -> Move.t option)  (* player B *)
  -> (int * int * int)                (* (A wins, B wins, draws) *)
