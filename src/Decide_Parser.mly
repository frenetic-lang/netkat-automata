%{
open Decide_Ast
open Decide_Ast.Term
open Decide_Ast.Formula
%}

%token <string> VAR
%token <string> STRING
%token ZERO ONE DUP
%token PLUS TIMES STAR INTER
%token NOT
%token LPAREN RPAREN
%token EQ NEQ EQUIV LE ASSG
%token EOL

%nonassoc EQ LE /* lowest precedence */
%left PLUS
%left TIMES VAR ZERO ONE DUP LPAREN
%nonassoc NOT STAR /* highest precedence */

%start formula_main term_main  /* entry points */
%type <Decide_Ast.Formula.t> formula_main
%type <Decide_Ast.Term.t> term_main

%%

formula_main:
  | formula EOL { $1 }
  | EOL { raise Decide_Deriv.Empty } 
;

term_main:
  | term EOL { $1 }
;

term:
  | VAR ASSG STRING { assg (Decide_Util.Field.of_string $1) (Decide_Util.Value.of_string $3) }
  | VAR EQ STRING   { test (Decide_Util.Field.of_string $1) (Decide_Util.Value.of_string $3) }
  | VAR NEQ STRING  { not (test (Decide_Util.Field.of_string $1) (Decide_Util.Value.of_string $3)) }
  | ZERO            { zero }
  | ONE             { one }
  | DUP             { dup } 
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { plus (TermSet.of_list [$1; $3]) }
  | term TIMES term { times [$1; $3] }
  | term INTER term { intersection (TermSet.of_list [$1; $3]) }
  | term STAR       { star $1 }
  | NOT term        { not $2 }
  | term term %prec TIMES { times [$1; $2] }
;

formula:
  | term EQUIV term { make_eq $1 $3 }
  | term LE term    { make_le $1 $3 }
;
