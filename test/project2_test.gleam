import gleam/dict
import gleeunit
import gleeunit/should
import project2.{get_command_line_args, parse_algorithm, parse_topology}
import topology
import utils

pub fn main() -> Nil {
  gleeunit.main()
}

// Test parsing topologies
pub fn parse_topology_full_test() {
  parse_topology("full")
  |> should.equal(Ok(topology.Full))
}

pub fn parse_topology_3d_test() {
  parse_topology("3D")
  |> should.equal(Ok(topology.ThreeDGrid))
}

pub fn parse_topology_line_test() {
  parse_topology("line")
  |> should.equal(Ok(topology.Line))
}

pub fn parse_topology_imp3d_test() {
  parse_topology("imp3D")
  |> should.equal(Ok(topology.ImperfectThreeDGrid))
}

pub fn parse_topology_invalid_test() {
  parse_topology("invalid")
  |> should.be_error()
}

// Test parsing algorithms
pub fn parse_algorithm_gossip_test() {
  parse_algorithm("gossip")
  |> should.equal(Ok(project2.Gossip))
}

pub fn parse_algorithm_push_sum_test() {
  parse_algorithm("push-sum")
  |> should.equal(Ok(project2.PushSum))
}

pub fn parse_algorithm_invalid_test() {
  parse_algorithm("invalid")
  |> should.be_error()
}

// Test command line args parsing
pub fn get_command_line_args_valid_test() {
  get_command_line_args(["100", "full", "gossip", "0.1"])
  |> should.equal(Ok(#(100, topology.Full, project2.Gossip, 0.1)))
}

pub fn get_command_line_args_invalid_num_nodes_test() {
  get_command_line_args(["abc", "full", "gossip", "0.0"])
  |> should.be_error()
}

pub fn get_command_line_args_invalid_topology_test() {
  get_command_line_args(["100", "invalid", "gossip", "0.0"])
  |> should.be_error()
}

pub fn get_command_line_args_invalid_algorithm_test() {
  get_command_line_args(["100", "full", "invalid", "0.0"])
  |> should.be_error()
}

pub fn get_command_line_args_invalid_failure_rate_test() {
  get_command_line_args(["100", "full", "gossip", "abc"])
  |> should.be_error()
}

pub fn get_command_line_args_wrong_count_test() {
  get_command_line_args(["100", "full"])
  |> should.be_error()
}

// Test topology creation
pub fn create_full_topology_test() {
  let result = topology.create_topology(3, topology.Full)
  result |> should.be_ok()

  let neighbor_map = result |> should.be_ok()
  dict.get(neighbor_map, 0) |> should.equal(Ok([1, 2]))
  dict.get(neighbor_map, 1) |> should.equal(Ok([0, 2]))
  dict.get(neighbor_map, 2) |> should.equal(Ok([0, 1]))
}

pub fn create_line_topology_test() {
  let result = topology.create_topology(3, topology.Line)
  result |> should.be_ok()

  let neighbor_map = result |> should.be_ok()
  dict.get(neighbor_map, 0) |> should.equal(Ok([1]))
  dict.get(neighbor_map, 1) |> should.equal(Ok([0, 2]))
  dict.get(neighbor_map, 2) |> should.equal(Ok([1]))
}

pub fn create_line_topology_single_node_test() {
  let result = topology.create_topology(1, topology.Line)
  result |> should.be_ok()

  let neighbor_map = result |> should.be_ok()
  dict.get(neighbor_map, 0) |> should.equal(Ok([]))
}

// Test utils
pub fn elapsed_ms_positive_test() {
  utils.elapsed_ms(100, 200)
  |> should.equal(100)
}

pub fn elapsed_ms_zero_test() {
  utils.elapsed_ms(200, 200)
  |> should.equal(0)
}

pub fn elapsed_ms_negative_diff_test() {
  utils.elapsed_ms(200, 100)
  |> should.equal(0)
}
