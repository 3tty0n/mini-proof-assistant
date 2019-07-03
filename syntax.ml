type t =
  | Var of string
  | Fun of t * t
  | ImpI of t * t
  | ImpE of t * t
  | Assume of t * t
[@@deriving show]
