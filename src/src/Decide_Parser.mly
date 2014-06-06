%{
open Decide_Ast
open Decide_Ast.Term
open Decide_Ast.Formula
%}

%token <string> VAR
%token <string> STRING
%token ZERO ONE DUP
%token PLUS TIMES STAR
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
  | EOL { raise Decide_Ast.Empty } 
;

term_main:
  | term EOL { $1 }
;

term:
  | VAR ASSG STRING { make_assg (Decide_Util.Field.of_string $1, Decide_Util.Value.of_string $3) }
  | VAR EQ STRING   { make_test (Decide_Util.Field.of_string $1, Decide_Util.Value.of_string $3) }
  | VAR NEQ STRING  { make_not (make_test (Decide_Util.Field.of_string $1, Decide_Util.Value.of_string $3)) }
  | ZERO            { make_zero ()}
  | ONE             { make_one ()}
  | DUP             { make_dup ()} 
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { make_plus (TermSet.of_list [$1; $3]) }
  | term TIMES term { make_times [$1; $3] }
  | term STAR       { make_star $1 }
  | NOT term        { make_not $2 }
  | term term %prec TIMES { make_times [$1; $2] }
;

formula:
  | term EQUIV term { make_eq $1 $3 }
  | term LE term    { make_le $1 $3 }
;
