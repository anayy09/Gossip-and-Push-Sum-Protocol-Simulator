# Project 2: Gossip and Push-Sum Simulations

This project implements gossip and push-sum algorithms for distributed systems using Gleam's actor model.

## Features

- **Gossip Algorithm**: Rumor propagation with convergence detection
- **Push-Sum Algorithm**: Average computation with ratio stability detection
- **Network Topologies**:
  - Full: Every node connected to all others
  - 3D Grid: Nodes in 3D cube with adjacent neighbors
  - Line: Linear arrangement with 2 neighbors max
  - Imperfect 3D Grid: 3D grid plus one random connection
- **Actor-based Implementation**: Uses Gleam OTP for concurrent processing
- **Convergence Timing**: Measures time to reach consensus

## Requirements

- Gleam 1.12.0+
- Erlang/OTP 26+

## Installation

1. Install Gleam: [https://gleam.run/getting-started/](https://gleam.run/getting-started/)
2. Clone/download the project
3. Run `gleam deps download`

## Usage

```bash
gleam run <num_nodes> <topology> <algorithm>
```

### Parameters

- `num_nodes`: Number of nodes (integer)
- `topology`: `full`, `3D`, `line`, `imp3D`
- `algorithm`: `gossip`, `push-sum`

### Examples

```bash
gleam run 10 full gossip
gleam run 27 3D push-sum
gleam run 100 line gossip
gleam run 64 imp3D push-sum
```

## Output

The program outputs the convergence time in milliseconds:

```text
1247
```

## Project Structure

- `src/project2.gleam`: Main entry point
- `src/gossip.gleam`: Gossip algorithm implementation
- `src/push_sum.gleam`: Push-sum algorithm implementation
- `src/topology.gleam`: Network topology generators