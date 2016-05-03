open Core.Std
open Async.Std

module Ast = Frenetic_Decide_Ast
module Measurement = Frenetic_Decide_Measurement
module Net = Frenetic_Network.Net

let main (topo_file: string) : unit Deferred.t =
  let topo = Net.Parse.from_dotfile topo_file in
  let num_vertexes = Net.Topology.num_vertexes topo in
  let num_switches =
    Net.Topology.vertexes topo
    |> Net.Topology.VertexSet.count ~f:(fun n ->
        let label = Net.Topology.vertex_to_label topo n in
        Frenetic_Network.Node.(device label = Switch))
  in
  print_endline (Net.Pretty.to_string topo);
  print_endline "";
  print_endline (Ast.Term.to_string (Measurement.term_of_topology topo));
  print_endline "";
  print_endline (Ast.Term.to_string (Measurement.in_of_topology topo));
  print_endline "";
  print_endline (Ast.Term.to_string (Measurement.out_of_topology topo));
  print_endline "";
  printf "%d vertices\n" num_vertexes;
  printf "%d switches\n" num_switches;
  return ()

let () =
  Command.(async
    ~summary:"Compile dot topologies into NetKAT terms"
    Spec.(empty +> anon ("topo_dot_file" %: file))
    (fun topo_file () -> main topo_file)
  ) |> Command.run
