open Decide_Util
module Ast = Decide_Ast

module Deriv' = Decide_Deriv.BDDMixed

let print_Ematrix t =
  let print_point p =
    Printf.printf "%s\n" (Ast.point_to_string p) in
  let tvals = Ast.Term.values t in
  ignore (set_univ [tvals]);
  let module DerivTerm = (Deriv' ()) in
  let t' = DerivTerm.make_term (Ast.TermSet.singleton t) in
  let q_E = DerivTerm.get_e t' in
  let points = DerivTerm.EMatrix.fold q_E ~init:[] ~f:(fun a p -> p :: a) in
  print_endline "Points:";
  List.iter print_point points

exception ParseError of int * int * string

let parse (s : string) = 
  let lexbuf = Lexing.from_string s in
  (try
     Decide_Parser.formula_main Decide_Lexer.token lexbuf
   with
     | Parsing.Parse_error ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let token = Lexing.lexeme lexbuf in
    raise (ParseError (line, char, token)))

let process (input : string) : unit =
  try
    match parse input with
    | Ast.Formula.Eval t -> print_Ematrix t
    | _                  -> print_endline "Invalid format"
  with
  | Decide_Deriv.Empty -> 
    ()
  | Decide_Lexer.LexError s -> 
    Printf.printf "Lex Error: %s\n" s
  | ParseError (l, ch, t) ->
    Printf.printf "Syntax error at line %d, char %d, token \'%s\'\n" l ch t

(* read from a file *)
let load (filename : string) : string option =
  let rec get_contents contents file =
    let input_line_option file =
      try Some (input_line file) with End_of_file -> None in
    match (input_line_option file) with
    | Some x -> get_contents (x :: contents) file
    | None -> contents in
  try
    let file = open_in filename in
    let result = get_contents [] file in
    close_in file;
    Some (String.concat " " (List.rev result))
  with Sys_error msg ->
    print_endline msg; None

let process_file (filename : string) : unit =
  match (load filename) with
  | Some s -> process s
  | None -> ()

let main () =
  if (Array.length Sys.argv) > 1 then
    let args = List.tl (Array.to_list Sys.argv) in
    List.iter process_file args
  else print_endline "Please enter a filename"

let _ = main ()
