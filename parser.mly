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

expr:
  | VAR
    { Syntax.Var ($1) }
  | FUN LPAREN expr RPAREN
    { Syntax.Fun ($3) }
  | IMPI LPAREN expr COMMA expr RPAREN
    { Syntax.ImpI ($3, $5) }
  | IMPE LPAREN expr COMMA expr RPAREN
    { Syntax.ImpE ($3, $5) }
  | ASSUME LPAREN expr COMMA  expr RPAREN
    { Syntax.Assume ($3, $5) }
