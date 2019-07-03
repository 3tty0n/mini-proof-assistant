type t =
  | Var of string
  | Fun of t
  | ImpI of t * t
  | ImpE of t * t
