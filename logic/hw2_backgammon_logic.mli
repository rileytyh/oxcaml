open! Core

module Player_kind : sig
  type t =
    | White
    | Black
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Location : sig
  type t =
    | Point of int
    | Bar
    | Off
  [@@deriving sexp, compare, equal]
end

module Move : sig
  type t =
    { from_ : Location.t
    ; die : int
    }
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t; dice_left : int list }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
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

  module Create_error : sig
    type t =
      | Bad_board_size
    [@@deriving sexp, compare, equal]
  end

  val create : unit -> (t, Create_error.t) Result.t

  module Move_error : sig
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

  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
end
