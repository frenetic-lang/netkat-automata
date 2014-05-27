%{
module Decide_Ast' = Decide_Ast.Ast(Decide_Ast.DummyUniv)
open Decide_Ast'
open Decide_Ast'.Term
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
%type <Decide_Ast.Ast(Decide_Ast.DummyUniv).formula> formula_main
%type <Decide_Ast.Ast(Decide_Ast.DummyUniv).term> term_main

%%

formula_main:
  | formula EOL { $1 }
  | EOL { raise Decide_Ast.Empty } 
;

term_main:
  | term EOL { $1 }
;

term:
  | VAR ASSG STRING { Assg ( default_uid, Decide_Util.Field.of_string $1, Decide_Util.Value.of_string  $3, None) }
  | VAR EQ STRING   { Test ( default_uid, Decide_Util.Field.of_string $1, Decide_Util.Value.of_string $3, None) }
  | VAR NEQ STRING  { Not (default_uid, Test ( default_uid, Decide_Util.Field.of_string  $1, Decide_Util.Value.of_string $3, None), None) }
  | ZERO            { Zero (default_uid, None) }
  | ONE             { One (default_uid, None)}
  | DUP             { Dup (default_uid, None)}
  | LPAREN term RPAREN { $2 }
  | term PLUS term  { Plus (default_uid, BatSet.PSet.of_list [$1; $3], None) }
  | term TIMES term { Times (default_uid, [$1; $3], None) }
  | term STAR       { Star (default_uid, $1, None) }
  | NOT term        { Not (default_uid, $2, None) }
  | term term %prec TIMES { Times (default_uid, [$1; $2], None) }
;

formula:
  | term EQUIV term { Eq ($1, $3) }
  | term LE term    { Le ($1, $3) }
;
