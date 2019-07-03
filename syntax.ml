type t =
  | Var of string
  | ImpI of t * t
  | ImpE of t * t
  | Fun of t * t
  | Assume of t * t
[@@deriving show]
