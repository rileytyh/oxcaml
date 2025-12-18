[@@@ocaml.warning "-32"]

type player_kind =
  | White
  | Black

type location =
  | Point of int  (* 1..24 *)
  | Bar
  | Off

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind

type stack =
  { owner : player_kind
  ; count : int
  }

type board = stack option array  (* index 0..23 corresponds to Point 1..24 *)

type game_state =
  { board : board
  ; bar_white : int
  ; bar_black : int
  ; off_white : int
  ; off_black : int
  ; dice : int * int
  ; decision : decision
  }

type move =
  { from_ : location
  ; to_ : location
  ; die : int
  }

let empty_board () : board = Array.make 24 None

let initial_board : board =
  [|
    Some { owner = Black; count = 2 };
    None; None; None; None;
    Some { owner = White; count = 5 };
    None;
    Some { owner = White; count = 3 };
    None; None; None;
    Some { owner = Black; count = 5 };
    Some { owner = White; count = 5 };
    None; None; None;
    Some { owner = Black; count = 3 };
    None;
    Some { owner = Black; count = 5 };
    None; None; None; None;
    Some { owner = White; count = 2 };
  |]

let initial_state : game_state =
  { board = initial_board
  ; bar_white = 0
  ; bar_black = 0
  ; off_white = 0
  ; off_black = 0
  ; dice = (6, 1)
  ; decision = In_progress { whose_turn = White }
  }

let move_opening : move =
  { from_ = Point 24; to_ = Point 18; die = 6 }

let state_after_opening : game_state =
  let b = Array.copy initial_board in
  b.(23) <- Some { owner = White; count = 1 };
  b.(17) <- Some { owner = White; count = 1 };
  { board = b
  ; bar_white = 0
  ; bar_black = 0
  ; off_white = 0
  ; off_black = 0
  ; dice = (3, 2)
  ; decision = In_progress { whose_turn = Black }
  }

let mid_board : board =
  let b = empty_board () in
  b.(3) <- Some { owner = Black; count = 1 };
  b.(4) <- Some { owner = White; count = 2 };
  b.(7) <- Some { owner = White; count = 3 };
  b.(11) <- Some { owner = Black; count = 2 };
  b.(12) <- Some { owner = White; count = 4 };
  b.(18) <- Some { owner = Black; count = 3 };
  b.(23) <- Some { owner = White; count = 1 };
  b

let mid_state : game_state =
  { board = mid_board
  ; bar_white = 0
  ; bar_black = 0
  ; off_white = 5
  ; off_black = 3
  ; dice = (1, 4)
  ; decision = In_progress { whose_turn = White }
  }

let move_hit : move =
  { from_ = Point 5; to_ = Point 4; die = 1 }

let state_after_hit : game_state =
  let b = Array.copy mid_board in
  b.(4) <- Some { owner = White; count = 1 };
  b.(3) <- Some { owner = White; count = 1 };
  { board = b
  ; bar_white = 0
  ; bar_black = 1
  ; off_white = 5
  ; off_black = 3
  ; dice = (6, 2)
  ; decision = In_progress { whose_turn = Black }
  }

let near_win_board : board =
  let b = empty_board () in
  b.(0) <- Some { owner = White; count = 1 };
  b

let near_win_state : game_state =
  { board = near_win_board
  ; bar_white = 0
  ; bar_black = 0
  ; off_white = 14
  ; off_black = 10
  ; dice = (1, 5)
  ; decision = In_progress { whose_turn = White }
  }

let move_to_win : move =
  { from_ = Point 1; to_ = Off; die = 1 }

let terminal_state : game_state =
  let b = Array.copy near_win_board in
  b.(0) <- None;
  { board = b
  ; bar_white = 0
  ; bar_black = 0
  ; off_white = 15
  ; off_black = 10
  ; dice = (1, 5)
  ; decision = Winner White
  }

let _ =
  ignore mid_state;
  ignore near_win_state
;;
