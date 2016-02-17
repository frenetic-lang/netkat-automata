open Core.Std
open Async.Std

module Pretty = Frenetic_NetKAT_Pretty

let main (filename: string) () : unit Deferred.t =
  Reader.file_contents filename >>| fun json_policy_string ->
  let policy = Frenetic_NetKAT_Json.policy_from_json_string json_policy_string in
  let policy_string = Pretty.string_of_policy policy in
  let fixed_string = (Str.global_replace (Str.regexp "ipDst") "ip4Dst" policy_string) in
  printf "filter ethTyp=0x800; (\n%s\n)\n" fixed_string

let () =
  Command.(async
    ~summary:"Compile dot topologies into NetKAT terms"
    Spec.(
      empty
      +> anon ("JSON_Policy_File" %: file)
    )
    main
  ) |> Command.run
