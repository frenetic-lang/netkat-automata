open Core.Std
open Async.Std

module Ast = Frenetic_Decide_Ast
module Deriv = Frenetic_Decide_Deriv
module DerivTerm = Deriv.BDDDeriv
module Measurement = Frenetic_Decide_Measurement
module Util = Frenetic_Decide_Util

(* [base f] returns the basename of [f] without any file extensions. *)
let base (filename: string) : string =
  Filename.basename filename
  |> Filename.split_extension
  |> fst

let p_main (p_files: string list) : unit Deferred.t =
  let p_size (p_file: string) : unit Deferred.t =
    Reader.file_contents p_file >>| fun policy_string ->
    Frenetic_NetKAT_Json.policy_from_json_string policy_string
    |> Frenetic_NetKAT_Semantics.size
    |> printf "%s, %d\n" (base p_file)
  in
  Deferred.List.iter ~f:p_size p_files

let t_main (t_files: string list) : unit Deferred.t =
  let t_size (t_file: string) : unit Deferred.t =
    failwith "TODO"
  in
  Deferred.List.iter ~f:t_size t_files

let p =
  Command.async
    ~summary:"Prints term size of policies."
    Command.Spec.(
      empty
      +> anon (sequence ("ps" %: file))
    )
    (fun p_files () -> p_main p_files)


let t =
  Command.async
    ~summary:"Prints term size of topologies."
    Command.Spec.(
      empty
      +> anon (sequence ("ts" %: file))
    )
    (fun t_files () -> t_main t_files)

let () =
  Command.group
    ~summary:"Prints term sizes of policies and topologies."
    [
      ("p", p);
      ("t", t);
    ]
  |> Command.run
