type token =
  | VAR of (string)
  | STRING of (string)
  | ZERO
  | ONE
  | DUP
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

open Parsing;;
# 1 "Parser/parser.mly"

open Ast
open Ast.Term
# 26 "Parser/parser.ml"
let yytransl_const = [|
  259 (* ZERO *);
  260 (* ONE *);
  261 (* DUP *);
  262 (* PLUS *);
  263 (* TIMES *);
  264 (* STAR *);
  265 (* NOT *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* EQUIV *);
  271 (* LE *);
  272 (* ASSG *);
  273 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\003\000\
\003\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\003\000\003\000\001\000\001\000\
\001\000\003\000\003\000\003\000\002\000\002\000\002\000\003\000\
\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\007\000\008\000\009\000\000\000\
\000\000\002\000\018\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\014\000\000\000\001\000\000\000\000\000\013\000\
\000\000\000\000\000\000\003\000\005\000\006\000\004\000\010\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\003\000\
\011\000\014\000\012\000\027\000"

let yysindex = "\003\000\
\055\255\121\255\000\000\246\254\000\000\000\000\000\000\121\255\
\121\255\000\000\000\000\251\254\070\255\000\000\010\255\021\255\
\031\255\032\255\000\000\090\255\000\000\121\255\121\255\000\000\
\121\255\121\255\001\255\000\000\000\000\000\000\000\000\000\000\
\111\255\001\255\101\255\101\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\255\000\000\000\000\000\000\000\000\000\000\
\075\255\040\255\020\255\035\255"

let yygindex = "\000\000\
\000\000\000\000\000\000\255\255"

let yytablesize = 131
let yytable = "\013\000\
\015\000\016\000\017\000\001\000\002\000\018\000\019\000\020\000\
\024\000\008\000\004\000\021\000\005\000\006\000\007\000\022\000\
\023\000\024\000\008\000\009\000\033\000\034\000\029\000\035\000\
\036\000\015\000\028\000\015\000\015\000\015\000\015\000\015\000\
\030\000\031\000\015\000\015\000\016\000\000\000\015\000\015\000\
\012\000\015\000\012\000\012\000\012\000\012\000\012\000\000\000\
\000\000\012\000\012\000\017\000\000\000\012\000\012\000\004\000\
\012\000\005\000\006\000\007\000\000\000\000\000\000\000\008\000\
\009\000\000\000\000\000\000\000\000\000\000\000\004\000\010\000\
\005\000\006\000\007\000\022\000\023\000\024\000\008\000\009\000\
\011\000\000\000\000\000\025\000\026\000\011\000\000\000\000\000\
\011\000\011\000\004\000\011\000\005\000\006\000\007\000\022\000\
\023\000\024\000\008\000\009\000\032\000\004\000\000\000\005\000\
\006\000\007\000\022\000\023\000\024\000\008\000\009\000\004\000\
\000\000\005\000\006\000\007\000\000\000\023\000\024\000\008\000\
\009\000\004\000\000\000\005\000\006\000\007\000\000\000\000\000\
\000\000\008\000\009\000"

let yycheck = "\001\000\
\002\000\012\001\013\001\001\000\002\000\016\001\008\000\009\000\
\008\001\009\001\001\001\017\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\022\000\023\000\002\001\025\000\
\026\000\001\001\017\001\003\001\004\001\005\001\006\001\007\001\
\002\001\002\001\010\001\011\001\017\001\255\255\014\001\015\001\
\001\001\017\001\003\001\004\001\005\001\006\001\007\001\255\255\
\255\255\010\001\011\001\017\001\255\255\014\001\015\001\001\001\
\017\001\003\001\004\001\005\001\255\255\255\255\255\255\009\001\
\010\001\255\255\255\255\255\255\255\255\255\255\001\001\017\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\006\001\255\255\255\255\014\001\015\001\011\001\255\255\255\255\
\014\001\015\001\001\001\017\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\001\001\255\255\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\001\001\
\255\255\003\001\004\001\005\001\255\255\007\001\008\001\009\001\
\010\001\001\001\255\255\003\001\004\001\005\001\255\255\255\255\
\255\255\009\001\010\001"

let yynames_const = "\
  ZERO\000\
  ONE\000\
  DUP\000\
  PLUS\000\
  TIMES\000\
  STAR\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  EQ\000\
  NEQ\000\
  EQUIV\000\
  LE\000\
  ASSG\000\
  EOL\000\
  "

let yynames_block = "\
  VAR\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 27 "Parser/parser.mly"
                ( _1 )
# 156 "Parser/parser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "Parser/parser.mly"
        ( raise Empty )
# 162 "Parser/parser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 32 "Parser/parser.mly"
             ( _1 )
# 169 "Parser/parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "Parser/parser.mly"
                    ( Assg (_1, _3) )
# 177 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "Parser/parser.mly"
                    ( Test (_1, _3) )
# 185 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "Parser/parser.mly"
                    ( Not (Test (_1, _3)) )
# 193 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "Parser/parser.mly"
                    ( Zero )
# 199 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "Parser/parser.mly"
                    ( One )
# 205 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "Parser/parser.mly"
                    ( Dup )
# 211 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 42 "Parser/parser.mly"
                       ( _2 )
# 218 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "Parser/parser.mly"
                    ( Plus (TermSet.from_list [_1; _3]) )
# 226 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "Parser/parser.mly"
                    ( Times [_1; _3] )
# 234 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 45 "Parser/parser.mly"
                    ( Star _1 )
# 241 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 46 "Parser/parser.mly"
                    ( Not _2 )
# 248 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 47 "Parser/parser.mly"
                          ( Times [_1; _2] )
# 256 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 51 "Parser/parser.mly"
                    ( Eq (_1, _3) )
# 264 "Parser/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 52 "Parser/parser.mly"
                    ( Le (_1, _3) )
# 272 "Parser/parser.ml"
               : 'formula))
(* Entry formula_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry term_main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let formula_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.formula)
let term_main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ast.term)
