import argv
import gleam/int
import gleam/io
import gleam/result
import gossip
import push_sum
import topology.{type Topology}

pub type Algorithm {
  Gossip
  PushSum
}

pub fn main() -> Nil {
  let args = argv.load().arguments
  case get_command_line_args(args) {
    Ok(#(num_nodes, topo, algorithm)) -> {
      case run_simulation(num_nodes, topo, algorithm) {
        Ok(time_taken) -> io.println(int.to_string(time_taken))
        Error(error) -> io.println("Error: " <> error)
      }
    }
    Error(error) -> io.println(error)
  }
}


fn get_command_line_args(
  args: List(String),
) -> Result(#(Int, Topology, Algorithm), String) {
  case args {
    [num_nodes_str, topology_str, algorithm_str] -> {
      use num_nodes <- result.try(
        int.parse(num_nodes_str)
        |> result.map_error(fn(_) {
          "Invalid number of nodes: " <> num_nodes_str
        }),
      )

      use topo <- result.try(parse_topology(topology_str))
      use algorithm <- result.try(parse_algorithm(algorithm_str))

      Ok(#(num_nodes, topo, algorithm))
    }
    _ ->
      Error(
        "Usage: gleam run <numNodes> <topology> <algorithm>\nExample: gleam run 100 full gossip",
      )
  }
}

fn parse_topology(topology_str: String) -> Result(Topology, String) {
  case topology_str {
    "full" -> Ok(topology.Full)
    "3D" -> Ok(topology.ThreeDGrid)
    "line" -> Ok(topology.Line)
    "imp3D" -> Ok(topology.ImperfectThreeDGrid)
    _ ->
      Error(
        "Invalid topology: "
        <> topology_str
        <> ". Must be one of: full, 3D, line, imp3D",
      )
  }
}

fn parse_algorithm(algorithm_str: String) -> Result(Algorithm, String) {
  case algorithm_str {
    "gossip" -> Ok(Gossip)
    "push-sum" -> Ok(PushSum)
    _ ->
      Error(
        "Invalid algorithm: "
        <> algorithm_str
        <> ". Must be one of: gossip, push-sum",
      )
  }
}

fn run_simulation(
  num_nodes: Int,
  topo: Topology,
  algorithm: Algorithm,
) -> Result(Int, String) {
  // Create topology
  use neighbor_map <- result.try(topology.create_topology(num_nodes, topo))

  case algorithm {
    Gossip -> gossip.run_gossip_simulation(num_nodes, neighbor_map)
    PushSum -> push_sum.run_push_sum_simulation(num_nodes, neighbor_map)
  }
}
