# Project 2: Gossip and Push-Sum Simulations

This project implements gossip and push-sum algorithms for distributed systems using Gleam's actor model.

## Team Members

- Anay Sinhal (68789243)
- Radhey Sharma (21089036)

## What is Working

- Gossip and Push-Sum algorithms implemented using Gleam's actor model for distributed simulations.
- Support for multiple network topologies: full, 3D grid, line, and imperfect 3D grid.
- Convergence detection mechanisms for both algorithms.
- Command-line interface for running simulations with configurable parameters.
- Measurement scripts (`measure.py`, `measure_bonus.py`) for automated performance analysis, data collection and for visualizing convergence times and failure impacts.
- Unit tests for key functions in `project2_test.gleam`.

## Largest Network Sizes

The simulations were tested at various sizes; below are the largest tested sizes present in `results.json` for each topology and algorithm, with the recorded convergence time for that size.

- **Gossip Algorithm** (largest tested size -> measured time):
  - Full: 10,000 nodes -> 54,420 ms
  - 3D Grid: 17,000 nodes -> 55,815 ms
  - Line: 10,000 nodes -> 70,001 ms
  - Imperfect 3D Grid: 12,000 nodes -> 15,217 ms

- **Push-Sum Algorithm** (largest tested size -> measured time):
  - Full: 10,000 nodes -> 74,963 ms
  - 3D Grid: 10,000 nodes -> 70,077 ms
  - Line: 10,000 nodes -> 70,730 ms
  - Imperfect 3D Grid: 11,000 nodes -> 28,792 ms

## Bonus: Failure Model Experiments

We also ran a set of experiments measuring the impact of node message-drop failures on convergence time. The raw results are saved in `results_bonus.json` and the two summary plots (one for Gossip and one for Push-Sum) are in the `plots/` folder.

Summary (1000 nodes, varying failure rate 0.0 → 0.5):

- Gossip: the `3D` and `imp3D` topologies remain fast for low failure rates but show catastrophic slowdowns around 0.3 failure rate in the provided data; `full` and `line` stay roughly at high constant times for the tested values in `results_bonus.json`.

- Push-Sum: the `full` topology is mostly stable; `3D`, `line`, and `imp3D` show substantial variance across failure rates in the provided runs (see `results_bonus.json` for exact numbers).

Files:

- `results_bonus.json` — JSON object with convergence times keyed by algorithm → topology → failure_rate.

- `plots/gossip_failure.png` — Gossip plot generated from `results_bonus.json`.

- `plots/pushsum_failure.png` — Push-Sum plot generated from `results_bonus.json`.

Reproduce locally:

1. Make sure Gleam and Erlang/OTP are installed and available in PATH.

1. Run the bonus measurement script (it calls the Gleam simulation with a failure model):

```powershell
python measure_bonus.py
```

1. The script writes `results_bonus.json` and the plots into `plots/`.

Notes:

- The experiments were run with 1000 nodes and failure rates sampled at [0.0, 0.1, 0.2, 0.3, 0.4, 0.5]. High measured times (≈32k–63k ms) indicate runs where many messages were dropped or the simulation hit the configured timeout. Inspect `results_bonus.json` to see exact values per topology and failure rate.

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

