type player_kind =
  | White
  | Black

type location =
  | Point of int
  | Bar
  | Off

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind

type stack =
  { owner : player_kind
  ; count : int
  }

type board = stack option array

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

val initial_state : game_state

val move_opening : move
val state_after_opening : game_state

val move_hit : move
val state_after_hit : game_state

val move_to_win : move
val terminal_state : game_state
