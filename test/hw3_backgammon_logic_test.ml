open! Core
open Backgammon_logic_library
open Hw2_backgammon_logic


let ok_exn result = Result.ok result |> Option.value_exn

let create_and_print () =
  let result = Game_state.create () in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t) Result.t)]
;;

let%expect_test "Game_state.create basic" =
  create_and_print ();
  [%expect
    {|
    (Ok
     ((board
       ((((owner Black) (count 2))) () () () () (((owner White) (count 5))) ()
        (((owner White) (count 3))) () () () (((owner Black) (count 5)))
        (((owner White) (count 5))) () () () (((owner Black) (count 3))) ()
        (((owner Black) (count 5))) () () () () (((owner White) (count 2)))))
      (bar_white 0) (bar_black 0) (off_white 0) (off_black 0)
      (decision (In_progress (whose_turn White) (dice_left (6 1))))))
    |}]
;;

let make_move_and_print game_state move =
  let result = Game_state.make_move game_state move in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let initial =
  Game_state.create () |> ok_exn
;;

let%expect_test "make_move: legal opening move (White from Point 24 with die 6)" =
  make_move_and_print initial { from_ = Point 24; die = 6 };
  [%expect
    {|
    (Ok
     ((board
       ((((owner Black) (count 2))) () () () () (((owner White) (count 5))) ()
        (((owner White) (count 3))) () () () (((owner Black) (count 5)))
        (((owner White) (count 5))) () () () (((owner Black) (count 3)))
        (((owner White) (count 1))) (((owner Black) (count 5))) () () () ()
        (((owner White) (count 1)))))
      (bar_white 0) (bar_black 0) (off_white 0) (off_black 0)
      (decision (In_progress (whose_turn White) (dice_left (1))))))
    |}]
;;

let%expect_test "make_move: die not available" =
  make_move_and_print initial { from_ = Point 24; die = 2 };
  [%expect {| (Error Die_not_available) |}]
;;

let%expect_test "make_move: illegal from point" =
  make_move_and_print initial { from_ = Point 3; die = 1 };
  [%expect {| (Error Illegal_from) |}]
;;

let%expect_test "make_move: bar must enter first" =
  let s =
    { initial with
      bar_white = 1
    ; decision = In_progress { whose_turn = White; dice_left = [ 6 ] }
    }
  in
  make_move_and_print s { from_ = Point 24; die = 6 };
  [%expect {| (Error Must_enter_from_bar) |}]
;;

let%expect_test "make_move: enter from bar (White, die 6 -> Point 19)" =
  let s =
    { initial with
      bar_white = 1
    ; decision = In_progress { whose_turn = White; dice_left = [ 6 ] }
    }
  in
  make_move_and_print s { from_ = Bar; die = 6 };
  [%expect
    {|
    (Error Blocked)
    |}]
;;

let%expect_test "make_move: blocked (destination has opponent >=2)" =
  let b = Array.copy initial.board in
  b.(17) <- Some { owner = Black; count = 2 }; (* Point 18 is blocked by Black *)
  let s =
    { initial with
      board = b
    ; decision = In_progress { whose_turn = White; dice_left = [ 6 ] }
    }
  in
  make_move_and_print s { from_ = Point 24; die = 6 };
  [%expect {| (Error Blocked) |}]
;;

let%expect_test "make_move: hit blot (destination has opponent count=1 -> goes to bar)" =
  let b = Array.copy initial.board in
  b.(17) <- Some { owner = Black; count = 1 }; (* Point 18 is a blot *)
  let s =
    { initial with
      board = b
    ; decision = In_progress { whose_turn = White; dice_left = [ 6 ] }
    }
  in
  make_move_and_print s { from_ = Point 24; die = 6 };
  [%expect
    {|
    (Ok
     ((board
       ((((owner Black) (count 2))) () () () () (((owner White) (count 5))) ()
        (((owner White) (count 3))) () () () (((owner Black) (count 5)))
        (((owner White) (count 5))) () () () (((owner Black) (count 3)))
        (((owner White) (count 1))) (((owner Black) (count 5))) () () () ()
        (((owner White) (count 1)))))
      (bar_white 0) (bar_black 1) (off_white 0) (off_black 0)
      (decision (In_progress (whose_turn Black) (dice_left ())))))
    |}]
;;

let%expect_test "make_move: bear off (simplified exact die)" =
  let b = Array.create ~len:24 None in
  b.(0) <- Some Game_state.{ owner = White; count = 1 };
  let s =
    Game_state.
      { board = b
      ; bar_white = 0
      ; bar_black = 0
      ; off_white = 14
      ; off_black = 0
      ; decision = In_progress { whose_turn = White; dice_left = [ 1 ] }
      }
  in
  make_move_and_print s { from_ = Point 1; die = 1 };
  [%expect
    {|
    (Ok
     ((board
       (() () () () () () () () () () () () () () () () () () () () () () () ()))
      (bar_white 0) (bar_black 0) (off_white 15) (off_black 0)
      (decision (Winner White))))
    |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed ~max_steps =
  let rec loop (state : Game_state.t) steps_left =
    if steps_left <= 0
    then state
    else (
      match (state : Game_state.t).decision with
      | Winner _ -> state
      | In_progress { whose_turn = _; dice_left } ->
        (match dice_left with
         | [] -> state
         | die :: _ ->
           let candidates =
             [ Move.{ from_ = Bar; die }
             ; Move.{ from_ = Point 24; die }
             ; Move.{ from_ = Point 13; die }
             ; Move.{ from_ = Point 8; die }
             ; Move.{ from_ = Point 6; die }
             ]
           in
           let next_states =
             List.filter_map candidates ~f:(fun mv ->
               Game_state.make_move state mv |> Result.ok)
           in
           (match List.random_element next_states with
            | None -> state
            | Some s' -> loop s' (steps_left - 1))))
  in
  Random.init random_seed;
  loop initial_state max_steps
;;

let%test "random walk should not crash" =
  let s =
    { initial with decision = In_progress { whose_turn = White; dice_left = [ 6; 1; 6; 1 ] } }
  in
  let _final = random_walk s ~random_seed:1 ~max_steps:50 in
  true
;;
