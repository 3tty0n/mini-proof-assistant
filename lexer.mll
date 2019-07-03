{
  open Parser

  exception Lexing_failed of string
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']


rule token = parse
| [' ' '\t' '\n' ]+ { token lexbuf }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| "impI" { IMPI }
| "impE" { IMPE }
| "fun" { FUN }
| "assume" { ASSUME }
| lower (digit|lower|upper|'_')* { Var (Lexing.lexeme lexbuf) }
| eof { EOF }
| _
    { raise (Lexing_failed (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start))) }

{

}
