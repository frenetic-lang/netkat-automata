open Core.Std
open Async.Std

let main (policy_file: string) : unit Deferred.t =
  Reader.file_contents policy_file >>| fun policy_string ->
  Frenetic_NetKAT_Json.policy_from_json_string policy_string
  |> Frenetic_NetKAT_Pretty.string_of_policy
  |> print_endline

let () =
  Command.(async
    ~summary:"Compile JSON policies into NetKAT terms"
    Spec.(empty +> anon ("policy_json_file" %: file))
    (fun policy_file () -> main policy_file)
  ) |> Command.run
