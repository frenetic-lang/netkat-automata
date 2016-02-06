open Core.Std
open Async.Std

module Ast = Decide_Ast
module Pretty = Frenetic_NetKAT_Pretty

let main (filename: string) () : unit Deferred.t =
  (Reader.file_contents filename >>| fun json_policy_string ->
  Frenetic_NetKAT_Json.policy_from_json_string json_policy_string) >>| (fun policy ->
      Pretty.string_of_policy policy) >>| (fun policy_string -> print_string ("filter ethTyp=0x800; (" ^ (Str.global_replace (Str.regexp "ipDst") "ip4Dst" policy_string) ^ ")"))

let () =
  Command.(async
    ~summary:"Compile dot topologies into NetKAT terms"
    Spec.(
      empty
      +> anon ("JSON Policy File" %: file)
    )
    main 
  ) |> Command.run
