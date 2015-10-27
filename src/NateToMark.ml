open Core.Std
open Async.Std

let main (policy_file: string) : unit Deferred.t =
  Reader.file_contents policy_file >>| fun policy_string ->
  Frenetic_NetKAT_Json.policy_from_json_string policy_string
  |> Decide_Measurement.term_of_policy
  |> Decide_Ast.Term.to_string
  |> print_endline

let () =
  Command.(async
    ~summary:"Convert standard NetKAT to measurement style NetKAT"
    Spec.(empty +> anon ("policy_json_file" %: file))
    (fun policy_file () -> main policy_file)
  ) |> Command.run
