import argv
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
    Ok(#(num_nodes, topo, algorithm)) -> {
      io.println(
        "Parsed args: num_nodes="
        <> int.to_string(num_nodes)
        <> ", topo="
        <> topology_to_string(topo)
        <> ", algorithm="
        <> algorithm_to_string(algorithm),
      )
      case run_simulation(num_nodes, topo, algorithm) {
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

fn get_command_line_args(
  args: List(String),
) -> Result(#(Int, Topology, Algorithm), String) {
  io.println("Parsing command line arguments")
  case args {
    [num_nodes_str, topology_str, algorithm_str] -> {
      io.println(
        "Arguments: num_nodes_str="
        <> num_nodes_str
        <> ", topology_str="
        <> topology_str
        <> ", algorithm_str="
        <> algorithm_str,
      )
      use num_nodes <- result.try(
        int.parse(num_nodes_str)
        |> result.map_error(fn(_) {
          "Invalid number of nodes: " <> num_nodes_str
        }),
      )
      io.println("Parsed num_nodes: " <> int.to_string(num_nodes))

      use topo <- result.try(parse_topology(topology_str))
      io.println("Parsed topology: " <> topology_to_string(topo))
      use algorithm <- result.try(parse_algorithm(algorithm_str))
      io.println("Parsed algorithm: " <> algorithm_to_string(algorithm))

      Ok(#(num_nodes, topo, algorithm))
    }
    _ ->
      Error(
        "Usage: gleam run <numNodes> <topology> <algorithm>\nExample: gleam run 100 full gossip",
      )
  }
}

fn parse_topology(topology_str: String) -> Result(Topology, String) {
  io.println("Parsing topology: " <> topology_str)
  case topology_str {
    "full" -> {
      io.println("Parsed full topology")
      Ok(topology.Full)
    }
    "3D" -> {
      io.println("Parsed 3D grid topology")
      Ok(topology.ThreeDGrid)
    }
    "line" -> {
      io.println("Parsed line topology")
      Ok(topology.Line)
    }
    "imp3D" -> {
      io.println("Parsed imperfect 3D grid topology")
      Ok(topology.ImperfectThreeDGrid)
    }
    _ ->
      Error(
        "Invalid topology: "
        <> topology_str
        <> ". Must be one of: full, 3D, line, imp3D",
      )
  }
}

fn parse_algorithm(algorithm_str: String) -> Result(Algorithm, String) {
  io.println("Parsing algorithm: " <> algorithm_str)
  case algorithm_str {
    "gossip" -> {
      io.println("Parsed gossip algorithm")
      Ok(Gossip)
    }
    "push-sum" -> {
      io.println("Parsed push-sum algorithm")
      Ok(PushSum)
    }
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
  io.println(
    "Running simulation with "
    <> int.to_string(num_nodes)
    <> " nodes, topology "
    <> topology_to_string(topo)
    <> ", algorithm "
    <> algorithm_to_string(algorithm),
  )
  // Create topology
  io.println("Creating topology")
  use neighbor_map <- result.try(topology.create_topology(num_nodes, topo))
  io.println("Topology created successfully")

  case algorithm {
    Gossip -> {
      io.println("Starting gossip simulation")
      gossip.run_gossip_simulation(num_nodes, neighbor_map)
    }
    PushSum -> {
      io.println("Starting push-sum simulation")
      push_sum.run_push_sum_simulation(num_nodes, neighbor_map)
    }
  }
}
