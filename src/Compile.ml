open Core.Std
open Async.Std

module Ast = Decide_Ast
module Deriv = Decide_Deriv
module DerivTerm = Deriv.BDDDeriv
module Measurement = Decide_Measurement
module Util = Decide_Util

type network_files = {
  ingress  : string;
  outgress : string;
  p        : string;
  t        : string;
}

exception ParseError of string * int * int * string

let parse parser_function (filename: string) (s: string) =
  let lexbuf = Lexing.from_string s in
  try
    parser_function Decide_Lexer.token lexbuf
  with
    | Parsing.Parse_error -> begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let char = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let token = Lexing.lexeme lexbuf in
      raise (ParseError (filename, line, char, token))
    end

let parse_term (filename: string) (s: string) : Ast.Term.t =
  parse Decide_Parser.term_main filename s

let parse_query (filename: string) (s: string) : Measurement.Query.t =
  parse Decide_Parser.query_main filename s

let term_of_file (filename: string) : Ast.Term.t Deferred.t =
  Reader.file_contents filename >>| parse_term filename

let query_of_file (filename: string) : Measurement.Query.t Deferred.t =
  Reader.file_contents filename >>| parse_query filename

let network_of_files (files: network_files) : Measurement.network Deferred.t =
  term_of_file files.ingress >>= fun ingress ->
  term_of_file files.outgress >>= fun outgress ->
  term_of_file files.p >>= fun p ->
  term_of_file files.t >>= fun t ->
  return ({ingress; outgress; p; t}: Measurement.network)

let term_to_points (t: Ast.Term.t) : Ast.point list = 
  let tvals = Ast.Term.values t in
  ignore (Util.set_univ [tvals]);
  let t' = DerivTerm.make_term (Ast.TermSet.singleton t) in
  let q_E = DerivTerm.get_e t' in
  let points = DerivTerm.EMatrix.fold q_E ~init:[] ~f:(fun a p -> p :: a) in
  points

let print_Ematrix (t: Ast.Term.t) : unit =
  let print_point p =
    printf "%s\n" (Ast.point_to_string p) in
  let points = term_to_points t in
  print_endline "\nPoints:";
  List.iter points ~f:print_point

let packet_to_json (pkt: Ast.packet) : Yojson.json =
  let assoc_pkt = Ast.FieldMap.to_alist pkt in
  let pkt_string_lst = List.map assoc_pkt ~f:(fun (x, y) -> 
    (Util.Field.to_string x, Util.Value.to_string y)) in
  let pkt_json = `Assoc (List.map pkt_string_lst ~f:(fun (x, y) ->
    (x, (`String (y))))) in
    pkt_json

let points_to_jsons (points: Ast.point list) : Yojson.json list =
  let fst_packets = List.map points ~f:fst in
  let json_mapped_packets = List.map fst_packets packet_to_json in
  json_mapped_packets

let print_json_packets (t: Ast.Term.t) : unit =
  let points = term_to_points t in
  let json_pkts = points_to_jsons points in
  print_endline "\nJSON Packets for Measurement:";
  List.iter json_pkts ~f:(fun json_pkt -> print_endline (Yojson.to_string json_pkt))

let packet_to_request (pkt: Ast.packet) : (string * string list) list =
  let assoc_pkt = Ast.FieldMap.to_alist pkt in
  let pkt_string_lst = List.map assoc_pkt ~f:(fun (x, y) -> 
    (Util.Field.to_string x, Util.Value.to_string y)) in
  let pkt_and_config_string_lst =
    ("type", "config_sketch")::("interface", "OUTPUT")::pkt_string_lst in
  let request = List.map pkt_and_config_string_lst ~f:(fun (x, y) -> (x, y::[])) in
  request

let points_to_requests (points: Ast.point list) : ((string * string list) list) list =
  let fst_packets = List.map points ~f:fst in
  let requests = List.map fst_packets packet_to_request in
  requests

let build_and_print (network: Measurement.network) (q: Measurement.Query.t) : unit Deferred.t =
  let compiled = Measurement.compile network q in
  print_endline (Measurement.Query.to_string q);
  print_endline (Ast.Term.to_string compiled);
  print_Ematrix compiled;
  print_json_packets compiled;
  return ()

let run_files (network: Measurement.network) (q_file: string) : unit Deferred.t =
  query_of_file q_file >>= build_and_print network

let run_repl (network: Measurement.network) : unit Deferred.t =
  let stdin = Lazy.force Reader.stdin in
  print_string "> ";
  Pipe.iter (Reader.lines stdin) ~f:(fun line ->
    build_and_print network (parse_query "stdin" line) >>| fun () ->
    print_string "> ";
  )

let main (files: network_files) (q_file: string option) : unit Deferred.t =
  network_of_files files >>= fun network ->
  match q_file with
  | Some q_file -> run_files network q_file
  | None        -> run_repl network

let spec =
  let open Command.Spec in
  empty
  +> anon ("ingress" %: file)
  +> anon ("outgress" %: file)
  +> anon ("p" %: file)
  +> anon ("t" %: file)
  +> anon (maybe ("q" %: file))

let () =
  Command.async
    ~summary:"Compile path queries to NetKAT terms"
    spec
    (fun ingress outgress p t q () -> main {ingress; outgress; p; t} q)
  |> Command.run
