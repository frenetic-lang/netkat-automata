open Core.Std
open Async.Std

module Ast = Decide_Ast
module Measurement = Decide_Measurement
module Net = Frenetic_Network.Net

let main (topo_file: string) : unit Deferred.t =
  let topo = Net.Parse.from_dotfile topo_file in
  print_endline (Net.Pretty.to_string topo);
  print_endline "";
  print_endline (Ast.Term.to_string (Measurement.term_of_topology topo));
  return ()

let () =
  Command.(async
    ~summary:"Compile dot topologies into NetKAT terms"
    Spec.(empty +> anon ("topo_dot_file" %: file))
    (fun topo_file () -> main topo_file)
  ) |> Command.run
