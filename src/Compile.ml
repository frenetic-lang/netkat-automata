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

let timed (thunk: unit -> 'a Deferred.t) : ('a * Time.Span.t) Deferred.t =
  let start = Time.now () in
  thunk () >>= fun x ->
  let stop = Time.now () in
  return (x, Time.diff stop start)

(* [base f] returns the basename of [f] without any file extensions. *)
let base (filename: string) : string =
  Filename.basename filename
  |> Filename.split_extension
  |> fst

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

let term_of_policy_file (filename: string) : Ast.Term.t Deferred.t =
  Reader.file_contents filename >>| fun policy_string ->
  Frenetic_NetKAT_Json.policy_from_json_string policy_string
  |> fun p -> Decide_Measurement.term_of_policy p

(* Returns a list of terms split on ipDst *)
let terms_of_policy_file_ipdst (filename: string) : (Ast.Term.t list) Deferred.t =
  Reader.file_contents filename >>| fun policy_string ->
  Frenetic_NetKAT_Json.policy_from_json_string policy_string
  |> Decide_Measurement.terms_of_policy_ipdst

(* returns [in, out, p] *)
let terms_of_topo_file (filename: string) : (Ast.Term.t * Ast.Term.t * Ast.Term.t) Deferred.t =
  let topo = Frenetic_Network.Net.Parse.from_dotfile filename in
  return (
    Measurement.in_of_topology topo,
    Measurement.out_of_topology topo,
    Measurement.term_of_topology topo
  )

let network_of_files (files: network_files) : Measurement.network Deferred.t =
  term_of_file files.ingress >>= fun ingress ->
  term_of_file files.outgress >>= fun outgress ->
  term_of_file files.p >>= fun p ->
  term_of_file files.t >>= fun t ->
  return Measurement.({ingress; outgress; p; t})

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

let jsons_of_term (t: Ast.Term.t) : Yojson.json list =
  let points = term_to_points t in
  points_to_jsons points

let print_json_packets (t: Ast.Term.t) : unit =
  let json_pkts = jsons_of_term t in
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
  let jsons = jsons_of_term compiled in
  Writer.save "query.txt" (Yojson.to_string (`List jsons)) >>= fun () ->
  return ()

let build_and_time (network: Measurement.network) (q: Measurement.Query.t) : unit Deferred.t =
  let compiled = Measurement.compile network q in
  let points = term_to_points compiled in
  printf "%d points\n" (List.length points);
  return ()

let run_files (network: Measurement.network) (q_file: string) f : unit Deferred.t =
  query_of_file q_file >>= f network

let print_files (network: Measurement.network) (q_file: string) : unit Deferred.t =
  run_files network q_file build_and_print

let time_files (topo_name: string) (network: Measurement.network) (q_file: string) : unit Deferred.t =
  query_of_file q_file >>= fun q ->
  timed (fun () ->
    let compiled = Measurement.compile network q in
    let points = term_to_points compiled in
    return (List.length points)
  ) >>= fun (num_points, time) ->
  printf "%s, %s, %f, %d\n" topo_name (base q_file) (Time.Span.to_ms time) num_points;
  return ()

let time_files_ipdst (topo_name: string) (networks: Measurement.network list) (q_file: string) : unit Deferred.t =
  query_of_file q_file >>= fun q ->
  let help sum network =
    let compiled = Measurement.compile network q in
    let points = term_to_points compiled in
    sum + (List.length points)
  in
  timed (fun () ->
    return (List.fold networks ~init:0 ~f:help)
  ) >>= fun (num_points, time) ->
  printf "%s, %s, %f, %d\n" topo_name (base q_file) (Time.Span.to_ms time) num_points;
  return ()

let run_repl (network: Measurement.network) f : unit Deferred.t =
  let stdin = Lazy.force Reader.stdin in
  print_string "> ";
  Pipe.iter (Reader.lines stdin) ~f:(fun line ->
    f network (parse_query "stdin" line) >>| fun () ->
    print_string "> ";
  )

let print_repl (network: Measurement.network) : unit Deferred.t =
  run_repl network build_and_print

let time_repl (network: Measurement.network) : unit Deferred.t =
  run_repl network build_and_time

let inptout_main (files: network_files) (q_file: string option) : unit Deferred.t =
  network_of_files files >>= fun network ->
  match q_file with
  | Some q_file -> print_files network q_file
  | None        -> print_repl network

let zoo_split (policy_file: string) (topo_file: string) (q_files: string list) : unit Deferred.t =
  terms_of_topo_file topo_file >>= fun (ingress, outgress, t) ->
  terms_of_policy_file_ipdst policy_file >>= fun plst ->
  let networks = List.map plst (fun p -> Measurement.({ingress; outgress; p; t})) in
  match q_files with
  | _::_ -> Deferred.List.iter q_files ~f:(time_files_ipdst (base policy_file) networks)
  | []   -> failwith "temp" (*time_repl network*)

let zoo_main (policy_file: string) (topo_file: string) (q_files: string list) : unit Deferred.t =
  terms_of_topo_file topo_file >>= fun (ingress, outgress, t) ->
  term_of_policy_file policy_file >>= fun p ->
  let network = Measurement.({ingress; outgress; p; t}) in
  match q_files with
  | _::_ -> Deferred.List.iter q_files ~f:(time_files (base policy_file) network)
  | []   -> time_repl network

let inptout =
  Command.async
    ~summary:"Compiles a network described by in, out, p and t NetKAT files."
    Command.Spec.(
      empty
      +> anon ("ingress" %: file)
      +> anon ("outgress" %: file)
      +> anon ("p" %: file)
      +> anon ("t" %: file)
      +> anon (maybe ("q" %: file))
    )
    (fun ingress outgress p t q () -> inptout_main {ingress; outgress; p; t} q)

let zoo =
  Command.async
    ~summary:"Compiles a network described by a DOT file and a JSON file."
    Command.Spec.(
      empty
      +> flag "-s" no_arg ~doc: "Split the policy based on IP destination."
      +> anon ("policy" %: file)
      +> anon ("topology" %: file)
      +> anon (sequence ("q" %: file))
    )
    (fun split policy_file topo_file q () ->
    if split then zoo_split policy_file topo_file q
    else zoo_main policy_file topo_file q)

let () =
  Command.group
    ~summary:"Felix Compiler"
    [
      ("inptout", inptout);
      ("zoo",     zoo);
    ]
  |> Command.run
