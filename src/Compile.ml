open Core.Std
open Async.Std

module Ast = Decide_Ast
module Deriv = Decide_Deriv
module DerivTerm = Deriv.BDDDeriv
module Measurement = Decide_Measurement
module Util = Decide_Util

(*===========================================================================*)
(* CONSTANTS                                                                 *)
(*===========================================================================*)
let default_output_query_file = "query.txt"

(*===========================================================================*)
(* TYPES                                                                     *)
(*===========================================================================*)
(* When a user invokes `Compile.native inptout in out p t`, the files in,
 * out, p, and t are parsed into a network_files record. *)
type network_files = {
  ingress  : string;
  outgress : string;
  p        : string;
  t        : string;
}

(* A split_network record is a variant of a Measurement.network record where
 * the policy has been split using Measurement.terms_of_policy_ipdst. *)
type split_network = {
  ingress:  Ast.Term.t;
  outgress: Ast.Term.t;
  ps:       Ast.Term.t list;
  t:        Ast.Term.t;
}

(* Command line flags *)
type flags = {
  no_split: bool
}

(*===========================================================================*)
(* HELPER FUNCTIONS                                                          *)
(*===========================================================================*)
(* [timed f] times and evaluates the thunk `f`.
 *
 *   timed (fun () -> 1 + 1) >>= fun (two, t) ->
 *   printf "computing 1 + 1 = %d took %d ms" two (Time.Span.to_ms t);
 *)
let timed (thunk: unit -> 'a Deferred.t) : ('a * Time.Span.t) Deferred.t =
  let start = Time.now () in
  thunk () >>= fun x ->
  let stop = Time.now () in
  return (x, Time.diff stop start)

(* [base f] returns the basename of [f] without any file extensions.
 *
 *   base foo/bar/baz.txt = baz
 *   base baz.txt = baz
 *   base baz = baz
 *)
let base (filename: string) : string =
  Filename.basename filename
  |> Filename.split_extension
  |> fst

(* Joins a split network and a policy into a normal network. *)
let join_network (split_network: split_network) (p: Ast.Term.t) : Measurement.network =
  Measurement.({
    ingress  = split_network.ingress;
    outgress = split_network.outgress;
    p        = p;
    t        = split_network.t;
  })

(*===========================================================================*)
(* PARSING, TRANSLATING                                                      *)
(*===========================================================================*)
(* The filename, line, column, and token of a parsing error. *)
exception ParseError of string * int * int * string

(* Try to parse lexbuf, derived from filename, with parser_function and throw
 * an exception if parsing fails. If lexbuf wasn't derived from a file,
 * filename can be any descriptive string. *)
let parse_exn parser_function lexbuf (filename: string) =
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

(* Generate a lexbuf from filename, and then parse it with parser_function. *)
let parse_file_exn parser_function (filename: string) =
  let file_channel = In_channel.create filename in
  parse_exn parser_function (Lexing.from_channel file_channel) filename

(* Generate a lexbuf from s, and then parse it with parser_function. *)
let parse_string_exn parser_function (s: string) =
  parse_exn parser_function (Lexing.from_string s) "string"

let term_of_file (filename: string) : Ast.Term.t Deferred.t =
  return (parse_file_exn Decide_Parser.term_main filename)

let query_of_string (s: string) : Measurement.Query.t Deferred.t =
  return (parse_string_exn Decide_Parser.query_main s)

let query_of_file (filename: string) : Measurement.Query.t Deferred.t =
  return (parse_file_exn Decide_Parser.query_main filename)

let policy_of_file (filename: string) : Frenetic_NetKAT.policy Deferred.t =
  In_channel.create filename
  |> Frenetic_NetKAT_Json.policy_from_json_channel
  |> return

let term_of_policy_file (filename: string) : Ast.Term.t Deferred.t =
  policy_of_file filename >>| Decide_Measurement.term_of_policy

(* Returns a list of terms split on ipDst *)
let terms_of_policy_file_ipdst (filename: string) : (Ast.Term.t list) Deferred.t =
  policy_of_file filename >>| Decide_Measurement.terms_of_policy_ipdst

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

(*===========================================================================*)
(* PRINTING                                                                  *)
(*===========================================================================*)
let print_list (xs: 'a list) (f: 'a -> string) : unit =
  List.iter xs ~f:(fun x -> printf "%s\n" (f x))

let print_terms (terms: Ast.Term.t list) : unit =
  print_list terms Ast.Term.to_string

let print_points (points: Ast.point list) : unit =
  print_list points Ast.point_to_string

let print_jsons (jsons: Yojson.json list) : unit =
  print_list jsons Yojson.to_string

(*===========================================================================*)
(* EXECUTE                                                                   *)
(*===========================================================================*)
(* [repl f] repeatedly reads a string [s] from the command line, parses [s]
 * into a query [q], and then calls [f q]. *)
let repl (f: Measurement.Query.t -> unit Deferred.t) : unit Deferred.t =
  let stdin = Lazy.force Reader.stdin in
  print_string "> ";
  Pipe.iter (Reader.lines stdin) ~f:(fun line ->
    query_of_string line >>= fun query ->
    f query >>| fun () ->
    print_string "> ";
  )

let inptout_f (network: Measurement.network) (q: Measurement.Query.t) : unit Deferred.t =
  let compiled = Measurement.compile network q in
  let points = term_to_points compiled in
  let jsons = points_to_jsons points in

  print_endline "Query";
  print_endline "=====";
  print_endline (Measurement.Query.to_string q);
  print_endline "";

  print_endline "Compiled Terms";
  print_endline "=============";
  print_terms [compiled];
  print_endline "";

  print_endline "(alpha, beta) pairs";
  print_endline "===================";
  print_points points;
  print_endline "";

  print_endline "(alpha, beta) JSON";
  print_endline "==================";
  print_jsons jsons;

  Writer.save default_output_query_file (Yojson.to_string (`List jsons))

let inptout_repl (network: Measurement.network) : unit Deferred.t =
  repl (inptout_f network)

let inptout_file (network: Measurement.network) (query_file: string) : unit Deferred.t =
  (query_of_file query_file) >>= inptout_f network

type timed_compilation = {
  terms:        Ast.Term.t list;
  compile_time: Time.Span.t;
  points:       Ast.point list;
  points_time:  Time.Span.t;
}

let timed_compile (network: split_network) (q: Measurement.Query.t)
                  : timed_compilation Deferred.t =
  let join_p p = join_network network p in
  let compile_p p = Measurement.compile (join_p p) q in
  timed (fun () -> return (List.map network.ps ~f:compile_p))
    >>= fun (terms, compile_time) ->
  timed (fun () -> return (List.concat_map terms ~f:term_to_points))
    >>= fun (points, points_time) ->
  return {terms; compile_time; points; points_time}

let zoo_repl (network: split_network) : unit Deferred.t =
  let f query =
    timed_compile network query >>= fun tc ->
    printf "%d terms, %d points:\n" (List.length tc.terms) (List.length tc.points);
    printf "  %f ms of compilation\n" (Time.Span.to_ms tc.compile_time);
    printf "  %f ms of generating alpha, beta pairs\n" (Time.Span.to_ms tc.points_time);

    let jsons = points_to_jsons tc.points in
    Writer.save default_output_query_file (Yojson.to_string (`List jsons))
  in
  repl f

let zoo_file (topo_name: string) (network: split_network) (query_file: string)
             : unit Deferred.t =
  let f query =
    timed_compile network query >>= fun tc ->
    let total_time_ms = Time.Span.(to_ms tc.compile_time +. to_ms tc.points_time) in
    printf "%s, %s, %f, %d\n"
      topo_name (base query_file) total_time_ms (List.length tc.points);
    let jsons = points_to_jsons tc.points in
    Writer.save default_output_query_file (Yojson.to_string (`List jsons))
  in
  query_of_file query_file >>= f

(*===========================================================================*)
(* MAIN                                                                      *)
(*===========================================================================*)
let inptout_main (files: network_files) (q_file: string option) : unit Deferred.t =
  network_of_files files >>= fun network ->
  match q_file with
  | Some q_file -> inptout_file network q_file
  | None        -> inptout_repl network

let zoo_main (flags: flags) (policy_file: string) (topo_file: string)
             (q_files: string list)
             : unit Deferred.t =
  terms_of_topo_file topo_file >>= fun (ingress, outgress, t) ->
  begin
    if flags.no_split then
      term_of_policy_file policy_file >>| fun p ->
      {ingress; outgress; ps=[p]; t}
    else
      terms_of_policy_file_ipdst policy_file >>| fun ps ->
      {ingress; outgress; ps; t}
  end >>= fun split_network ->
  match q_files with
  | _::_ -> Deferred.List.iter q_files ~f:(zoo_file (base topo_file) split_network)
  | []   -> zoo_repl split_network

(*===========================================================================*)
(* COMMAND LINE                                                              *)
(*===========================================================================*)
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
      +> flag ~aliases:["-ns"] "-nosplit" no_arg
           ~doc:"Do NOT split the network policy based on destination IP addr."
      +> anon ("policy" %: file)
      +> anon ("topology" %: file)
      +> anon (sequence ("q" %: file))
    )
    (fun no_split policy_file topo_file q () ->
       let flags = {no_split} in
       zoo_main flags policy_file topo_file q)

let () =
  Command.group
    ~summary:"Felix Compiler"
    [
      ("inptout", inptout);
      ("zoo",     zoo);
    ]
  |> Command.run
