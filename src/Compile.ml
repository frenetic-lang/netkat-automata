open Core.Std
open Async.Std

module Ast = Decide_Ast
module Deriv = Decide_Deriv
module DerivTerm = Deriv.BDDDeriv
module Measurement = Decide_Measurement
module Util = Decide_Util

exception ParseError of int * int * string

let parse (s: string) : Ast.Term.t =
  let lexbuf = Lexing.from_string s in
  try
    Decide_Parser.term_main Decide_Lexer.token lexbuf
  with
    | Parsing.Parse_error -> begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let token = Lexing.lexeme lexbuf in
      raise (ParseError (line, char, token))
    end

let print_Ematrix (t: Ast.Term.t) : unit =
  let print_point p =
    Printf.printf "%s\n" (Ast.point_to_string p) in
  let tvals = Ast.Term.values t in
  ignore (Util.set_univ [tvals]);
  let t' = DerivTerm.make_term (Ast.TermSet.singleton t) in
  let q_E = DerivTerm.get_e t' in
  let points = DerivTerm.EMatrix.fold q_E ~init:[] ~f:(fun a p -> p :: a) in
  print_endline "\nPoints:";
  List.iter points ~f:print_point

let term_of_file (filename: string) : Ast.Term.t Deferred.t =
  Reader.file_contents filename >>| parse

let query () : Measurement.Query.t =
  (* In the future, we should parse queries from files. For now, we write them
   * up by hand. *)
  let open Measurement.Predicate in
  let s = Util.Field.of_string "s" in
  let p = Util.Field.of_string "p" in
  let one = Util.Value.of_string "1" in
  let two = Util.Value.of_string "2" in
  Measurement.Query.Pred (And (Test (s, two), Test (p, one)))

let main ingress_file outgress p_file t_file _q_file () : unit Deferred.t =
  term_of_file ingress_file >>= fun ingress ->
  term_of_file outgress >>= fun outgress ->
  term_of_file p_file >>= fun p ->
  term_of_file t_file >>= fun t ->
  let q = query () in
  let network: Measurement.network = {ingress; outgress; p; t} in
  let compiled = Measurement.compile network q in
  print_endline (Ast.Term.to_string compiled);
  print_Ematrix compiled;
  return ()

let spec =
  let open Command.Spec in
  empty
  +> anon ("ingress" %: file)
  +> anon ("outgress" %: file)
  +> anon ("p" %: file)
  +> anon ("t" %: file)
  +> anon ("q" %: file)

let () =
  Command.async
    ~summary:"Compile path queries to NetKAT terms"
    spec
    main
  |> Command.run
