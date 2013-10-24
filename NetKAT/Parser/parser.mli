type token =
  | VAR of (string)
  | STRING of (string)
  | ZERO
  | ONE
  | PLUS
  | TIMES
  | STAR
  | NOT
  | LPAREN
  | RPAREN
  | EQ
  | NEQ
  | EQUIV
  | LE
  | ASSG
  | EOL

val formula_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.formula
val term_main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
