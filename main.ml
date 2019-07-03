open Proof

let main () =
  Lexing.from_channel stdin
  |> Parser.program (Lexer.token)
  |> Syntax.show |> print_endline

let () = main ()
