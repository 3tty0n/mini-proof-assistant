open Proof_assist

let print_result (x, y) =
  let x, y = Proof.string_of_t x, Proof.string_of_typ y in
  print_endline y;
  print_endline "because";
  print_string x; print_string " : "; print_string y;
  print_newline ()

let test1 () =
  let input = "impI(x,impI(y,impE(assume(y,fun(a,b)),assume(x,a))))" in
  Lexing.from_string input
  |> Parser.program (Lexer.token)
  |> Term.f
  |> Proof.f
  |> print_result

let main f =
  let ic = open_in f in
  try
    Lexing.from_channel ic
    |> Parser.program Lexer.token
    |> Term.f
    |> Proof.f
    |> print_result;
    close_in ic
  with e -> close_in ic; raise e

let () =
  main (Sys.argv.(1))
