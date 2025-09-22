import argv
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gossip
import push_sum
import topology.{type Topology}

pub type Algorithm {
  Gossip
  PushSum
}

fn topology_to_string(topo: Topology) -> String {
  case topo {
    topology.Full -> "full"
    topology.ThreeDGrid -> "3D"
    topology.Line -> "line"
    topology.ImperfectThreeDGrid -> "imp3D"
  }
}

fn algorithm_to_string(alg: Algorithm) -> String {
  case alg {
    Gossip -> "gossip"
    PushSum -> "push-sum"
  }
}

pub fn main() -> Nil {
  io.println("Starting main function")
  let args = argv.load().arguments
  io.println("Loaded arguments: " <> int.to_string(list.length(args)))
  case get_command_line_args(args) {
    Ok(#(num_nodes, topo, algorithm, failure_rate)) -> {
      io.println(
        "Parsed args: num_nodes="
        <> int.to_string(num_nodes)
        <> ", topo="
        <> topology_to_string(topo)
        <> ", algorithm="
        <> algorithm_to_string(algorithm)
        <> ", failure_rate="
        <> float.to_string(failure_rate),
      )
      case run_simulation(num_nodes, topo, algorithm, failure_rate) {
        Ok(time_taken) -> {
          io.println(
            "Simulation completed successfully, time taken: "
            <> int.to_string(time_taken),
          )
          io.println(int.to_string(time_taken))
        }
        Error(error) -> {
          io.println("Simulation error: " <> error)
          io.println("Error: " <> error)
        }
      }
    }
    Error(error) -> {
      io.println("Argument parsing error: " <> error)
      io.println(error)
    }
  }
}

pub fn get_command_line_args(
  args: List(String),
) -> Result(#(Int, Topology, Algorithm, Float), String) {
  case args {
    [num_nodes_str, topology_str, algorithm_str, failure_rate_str] -> {
      use num_nodes <- result.try(
        int.parse(num_nodes_str)
        |> result.map_error(fn(_) {
          "Invalid number of nodes: " <> num_nodes_str
        }),
      )

      use topo <- result.try(parse_topology(topology_str))
      use algorithm <- result.try(parse_algorithm(algorithm_str))
      use failure_rate <- result.try(
        float.parse(failure_rate_str)
        |> result.map_error(fn(_) {
          "Invalid failure rate: " <> failure_rate_str
        }),
      )

      Ok(#(num_nodes, topo, algorithm, failure_rate))
    }
    _ ->
      Error(
        "Usage: gleam run <numNodes> <topology> <algorithm> <failureRate>\nExample: gleam run 100 full gossip 0.1",
      )
  }
}

pub fn parse_topology(topology_str: String) -> Result(Topology, String) {
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

pub fn parse_algorithm(algorithm_str: String) -> Result(Algorithm, String) {
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
  failure_rate: Float,
) -> Result(Int, String) {
  // Create topology
  io.println("Creating topology")
  use neighbor_map <- result.try(topology.create_topology(num_nodes, topo))
  io.println("Topology created successfully")

  case algorithm {
    Gossip ->
      gossip.run_gossip_simulation(num_nodes, neighbor_map, failure_rate)
    PushSum ->
      push_sum.run_push_sum_simulation(num_nodes, neighbor_map, failure_rate)
  }
}
