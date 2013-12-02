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

open Parsing;;
# 1 "Parser/parser.mly"

open Ast
open Ast.Term
# 25 "Parser/parser.ml"
let yytransl_const = [|
  259 (* ZERO *);
  260 (* ONE *);
  261 (* PLUS *);
  262 (* TIMES *);
  263 (* STAR *);
  264 (* NOT *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* EQ *);
  268 (* NEQ *);
  269 (* EQUIV *);
  270 (* LE *);
  271 (* ASSG *);
  272 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\003\000\003\000\000\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\003\000\003\000\001\000\001\000\003\000\
\003\000\003\000\002\000\002\000\002\000\003\000\003\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\006\000\007\000\000\000\000\000\
\016\000\000\000\000\000\017\000\000\000\000\000\000\000\000\000\
\012\000\000\000\001\000\000\000\000\000\011\000\000\000\000\000\
\000\000\002\000\004\000\005\000\003\000\008\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\003\000\
\009\000\012\000\010\000\025\000"

let yysindex = "\001\000\
\098\255\098\255\000\000\249\254\000\000\000\000\098\255\098\255\
\000\000\251\254\051\255\000\000\009\255\007\255\019\255\028\255\
\000\000\070\255\000\000\098\255\098\255\000\000\098\255\098\255\
\027\255\000\000\000\000\000\000\000\000\000\000\089\255\027\255\
\080\255\080\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\255\000\000\000\000\000\000\000\000\000\000\056\255\037\255\
\015\255\029\255"

let yygindex = "\000\000\
\000\000\000\000\000\000\255\255"

let yytablesize = 107
let yytable = "\011\000\
\013\000\001\000\002\000\014\000\015\000\017\000\018\000\016\000\
\027\000\004\000\019\000\005\000\006\000\020\000\021\000\022\000\
\007\000\008\000\031\000\032\000\028\000\033\000\034\000\013\000\
\026\000\013\000\013\000\013\000\013\000\029\000\014\000\013\000\
\013\000\022\000\007\000\013\000\013\000\010\000\013\000\010\000\
\010\000\010\000\010\000\000\000\015\000\010\000\010\000\000\000\
\000\000\010\000\010\000\004\000\010\000\005\000\006\000\020\000\
\021\000\022\000\007\000\008\000\009\000\000\000\000\000\023\000\
\024\000\009\000\000\000\000\000\009\000\009\000\004\000\009\000\
\005\000\006\000\020\000\021\000\022\000\007\000\008\000\030\000\
\004\000\000\000\005\000\006\000\020\000\021\000\022\000\007\000\
\008\000\004\000\000\000\005\000\006\000\000\000\021\000\022\000\
\007\000\008\000\004\000\000\000\005\000\006\000\000\000\000\000\
\000\000\007\000\008\000"

let yycheck = "\001\000\
\002\000\001\000\002\000\011\001\012\001\007\000\008\000\015\001\
\002\001\001\001\016\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\020\000\021\000\002\001\023\000\024\000\001\001\
\016\001\003\001\004\001\005\001\006\001\002\001\016\001\009\001\
\010\001\007\001\008\001\013\001\014\001\001\001\016\001\003\001\
\004\001\005\001\006\001\255\255\016\001\009\001\010\001\255\255\
\255\255\013\001\014\001\001\001\016\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\005\001\255\255\255\255\013\001\
\014\001\010\001\255\255\255\255\013\001\014\001\001\001\016\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\001\001\255\255\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\001\001\255\255\003\001\004\001\255\255\006\001\007\001\
\008\001\009\001\001\001\255\255\003\001\004\001\255\255\255\255\
\255\255\008\001\009\001"

let yynames_const = "\
  ZERO\000\
  ONE\000\
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
# 147 "Parser/parser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 31 "Parser/parser.mly"
           ( _1 )
# 154 "Parser/parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "Parser/parser.mly"
                    ( Assg (_1, _3) )
# 162 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "Parser/parser.mly"
                    ( Test (_1, _3) )
# 170 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "Parser/parser.mly"
                    ( Not (Test (_1, _3)) )
# 178 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "Parser/parser.mly"
                    ( Zero )
# 184 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "Parser/parser.mly"
                    ( One )
# 190 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 40 "Parser/parser.mly"
                       ( _2 )
# 197 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 41 "Parser/parser.mly"
                    ( Plus (termset_from_list [_1; _3]) )
# 205 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 42 "Parser/parser.mly"
                    ( Times [_1; _3] )
# 213 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 43 "Parser/parser.mly"
                    ( Star _1 )
# 220 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "Parser/parser.mly"
                    ( Not _2 )
# 227 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 45 "Parser/parser.mly"
                          ( Times [_1; _2] )
# 235 "Parser/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 49 "Parser/parser.mly"
                    ( Eq (_1, _3) )
# 243 "Parser/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 50 "Parser/parser.mly"
                    ( Le (_1, _3) )
# 251 "Parser/parser.ml"
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
