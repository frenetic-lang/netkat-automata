open Core.Std
open Async.Std

module Ast = Decide_Ast
module Measurement = Decide_Measurement
module Net = Frenetic_Network.Net

let main prologue_file epilogue_file topo_file () : unit Deferred.t =
  let topo = Net.Parse.from_dotfile topo_file in
  print_string (Net.Pretty.to_mininet ~prologue_file ~epilogue_file topo);
  return ()

let () =
  Command.(async
    ~summary:"Compile dot topologies into NetKAT terms"
    Spec.(
      empty
      +> anon ("prologue_file" %: file)
      +> anon ("epilogue_file" %: file)
      +> anon ("topo_dot_file" %: file)
    )
    main
  ) |> Command.run
