%{
    open Syntax
%}

%token <string> VAR
%token LPAREN RPAREN
%token COMMA
%token IMPI
%token IMPE
%token ASSUME
%token FUN
%token EOF
%type <Syntax.t> program
%start program

%%

program:
  | expr EOF
    { $1 }
;
expr:
  | VAR
    { Var $1 }
  | FUN LPAREN expr COMMA expr RPAREN
    { Fun ($3, $5) }
  | IMPI LPAREN expr COMMA expr RPAREN
    { ImpI ($3, $5) }
  | IMPE LPAREN expr COMMA expr RPAREN
    { ImpE ($3, $5) }
  | ASSUME LPAREN expr COMMA expr RPAREN
    { Assume ($3, $5) }
;
