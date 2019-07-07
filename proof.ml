open Printf

type name = string

type typ =
  | V of string
  | F of typ * typ

let rec string_of_typ = function
  | V s -> sprintf "%s" s
  | F (t1, t2) -> sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)

type t =
  | Var of name
  | Assign of name * t
  | Lambda of t * t * t
  | Fun of t * t
  | App of t * t

let rec string_of_t = function
  | Var s -> s
  | Assign (s, t) -> sprintf "Assign (%s, %s)" s (string_of_t t)
  | Lambda (t1, t2, t3) -> sprintf "λ%s:%s . %s" (string_of_t t1) (string_of_t t2) (string_of_t t3)
  | Fun (t1, t2) -> sprintf "(%s -> %s)" (string_of_t t1) (string_of_t t2)
  | App (t1, t2) -> sprintf "%s %s" (string_of_t t1) (string_of_t t2)

exception Not_bound

type env = (t * t) list

let empty_env = []

let extend_env x v env = (x, v) :: env

let append_env env ls = env @ ls

let lookup x env =
  try List.assoc x env
  with Not_found -> raise Not_bound


let rec resolve id t = match t with
  | Term.Var (_) -> None
  | Term.Assume (x, y) when x = id -> Some y
  | Term.Assume (_, y) -> resolve id y
  | Term.ImpI (x, y) | Term.ImpE (x, y) | Term.Fun (x, y) ->
     begin match resolve id x with
       Some v -> Some v
     | None -> resolve id y
     end

exception ImpE_failed of string

let rec eval env = function
  | Term.Var name ->
     Var (name), V (name), env
  | Term.Fun (t1, t2) ->
     let t1, typ1, env = eval env t1 in
     let t2, typ2, env = eval env t2 in
     Fun (t1, t2), F (typ1, typ2), env
  | Term.Assume (t1, t2) ->
     let t1, _, env = eval env t1 in
     let t2, typ2, env = eval env t2 in
     begin match t1 with
     | Var _ ->
        let env = extend_env t1 t2 env in
        t1, typ2, env
     | _ -> assert false
     end
  | Term.ImpI (t1, t2) ->
     begin match resolve t1 t2 with
     | Some (t) ->
        let l, _, env = eval env t1 in
        let r, typ1, env = eval env t in
        let t2, typ2, env = eval env t2 in
        Lambda (l, r, t2), F (typ1, typ2), env
     | None -> raise Not_found
     end
  | Term.ImpE (t1, t2) ->
     let t1, _, env = eval env t1 in
     let t2, typ2, env = eval env t2 in
     assert (
         let x = lookup t1 env in
         let y = lookup t2 env in
         eprintf "x: %s, y: %s\n" (string_of_t x) (string_of_t y);
         match x with
         | Fun (a, _) -> a = y || raise @@ ImpE_failed (sprintf "ImpE %s %s cannot be allowed." (string_of_t x) (string_of_t y))
         | _ -> raise @@ ImpE_failed "something went wrong.");
     App (t1, t2), typ2, env

let f t =
  let x, y, _ = eval empty_env t in x, y
