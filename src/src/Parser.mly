%{
open Decide_Ast
open Decide_Ast.Term
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
%type <Decide_Ast.formula> formula_main
%type <Decide_Ast.term> term_main

%%

formula_main:
  | formula EOL { $1 }
  | EOL { raise Empty } 
;

term_main:
  | term EOL { $1 }
;

term:
  | VAR ASSG STRING { Assg ( $1,  $3) }
  | VAR EQ STRING   { Test ( $1,  $3) }
  | VAR NEQ STRING  { Not (Test ( $1,  $3)) }
  | ZERO            { Zero }
  | ONE             { One }
  | DUP             { Dup }
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { Plus (TermSet.from_list [$1; $3]) }
  | term TIMES term { Times [$1; $3] }
  | term STAR       { Star $1 }
  | NOT term        { Not $2 }
  | term term %prec TIMES { Times [$1; $2] }
;

formula:
  | term EQUIV term { Eq ($1, $3) }
  | term LE term    { Le ($1, $3) }
;
