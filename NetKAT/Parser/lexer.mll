{
open Parser
}
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']*
let string = ['a'-'z' 'A'-'Z' '0'-'9' '.']*
rule token = parse
    [' ' '\t'] { token lexbuf }     (* skip blanks *)
  | ['\n'] { EOL }
  | "drop" { ZERO }
  | "pass" { ONE }
  | ['a'-'z'] alphanumeric as id { VAR id }
  | string as value { STRING value }
  | ":="   { ASSG }
  | '+'    { PLUS }
  | ';'    { TIMES }
  | '*'    { STAR }
  | '~'    { NOT }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | "=="   { EQUIV }
  | '='    { EQ }
  | "!="   { NEQ }
  | '<'    { LE }
  | eof    { EOL }
