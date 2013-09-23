let parseTerm (s : string) =
  Parser.term_main Lexer.token (Lexing.from_string s)

let parseFormula (s : string) =
  Parser.formula_main Lexer.token (Lexing.from_string s)