open Printf

exception Term_error of string

type t =
  Var of string
| Fun of t * t
| Assume of t * t
| ImpI of t * t
| ImpE of t * t
[@@deriving show]

let rec f = function
  | Syntax.Var v -> Var v
  | Syntax.Fun (t1, t2) ->
     begin match t1 with
     | Syntax.Var (v) -> Fun (Var v, f t2)
     | _ -> raise @@ Term_error (sprintf "fun (%s, _) should be a variable." (Syntax.show t1))
     end
  | Syntax.ImpI (t1, t2) -> ImpI (f t1, f t2)
  | Syntax.ImpE (t1, t2) -> ImpE (f t1, f t2)
  | Syntax.Assume (t1, t2) ->
     begin match t1 with
     | Syntax.Var v -> Assume (Var v, f t2)
     | _ -> raise @@ Term_error (sprintf "fun (%s, _) should be a variable." (Syntax.show t1))
     end
