{
open Parser
exception LexError of string
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let num = ['0'-'9']+
let whitespace = ['\n' '\t' '\r' ' ']

rule token = parse
  | whitespace { token lexbuf }
  | "drop" { ZERO }
  | "pass" { ONE }
  | "dup"  { DUP }
  | id as id { VAR id }
  | "\""   { STRING (String.concat "" (string lexbuf)) }
  | num as num { STRING (string_of_int (int_of_string num)) }
	| "(*"   { comment lexbuf }
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
  | "<="   { LE }
  | '<'    { LE }
  | eof    { EOL }
	| _ as c { raise (LexError ("Unexpected character " ^ (String.make 1 c))) }

and string = parse
  | "\\\\" { "\\" :: string lexbuf }
  | "\\\"" { "\"" :: string lexbuf }
  | "\\n"  { "\n" :: string lexbuf }
  | "\\t"  { "\t" :: string lexbuf }
  | "\""   { [] }
  | eof    { raise (LexError "Unexpected end of input stream") }
  | _ as c { String.make 1 c :: string lexbuf }

and comment = parse
  | "*)"   { token lexbuf }
  | eof    { raise (LexError "Unexpected end of input stream") }
  | _      { comment lexbuf }