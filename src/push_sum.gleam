import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/result
import topology.{type NeighborMap}

pub type PushSumMessage {
  PushSumPair(s: Float, w: Float)
  StartPushSum
  UpdateNeighborSubjects(subjects: Dict(Int, Subject(PushSumMessage)))
  Tick
  CheckConvergence(reply_to: Subject(Bool))
  GetStatus(reply_to: Subject(PushSumStatus))
  Shutdown
}

pub type PushSumStatus {
  PushSumStatus(
    node_id: Int,
    s: Float,
    w: Float,
    ratio: Float,
    is_active: Bool,
    consecutive_stable: Int,
  )
}

pub type PushSumState {
  PushSumState(
    node_id: Int,
    neighbors: List(Int),
    neighbor_subjects: Dict(Int, Subject(PushSumMessage)),
    s: Float,
    w: Float,
    previous_ratios: List(Float),
    is_active: Bool,
    consecutive_stable_count: Int,
    convergence_threshold: Float,
    stability_rounds: Int,
    rng: Int,
  )
}

const mod_ = 2_147_483_647

fn next_rng(r: Int) -> Int {
  let v = { r * 1_103_515_245 + 12_345 } % mod_
  case v < 0 {
    True -> -v
    False -> v
  }
}

fn pick_index(len: Int, rng: Int) -> #(Int, Int) {
  let r2 = next_rng(rng)
  let idx = case len {
    0 -> 0
    _ -> int.absolute_value(r2) % len
  }
  #(idx, r2)
}

pub fn run_push_sum_simulation(
  num_nodes: Int,
  neighbor_map: NeighborMap,
) -> Result(Int, String) {
  use actors <- result.try(start_push_sum_actors(num_nodes, neighbor_map))

  case dict.get(actors, 0) {
    Ok(first_actor) -> {
      process.send(first_actor, StartPushSum)
      let time_taken = wait_for_push_sum_convergence(actors)
      shutdown_push_sum_actors(actors)
      Ok(time_taken)
    }
    Error(_) -> Error("Failed to start push-sum simulation")
  }
}

fn start_push_sum_actors(
  num_nodes: Int,
  neighbor_map: NeighborMap,
) -> Result(Dict(Int, Subject(PushSumMessage)), String) {
  let actor_results =
    list.range(0, num_nodes - 1)
    |> list.map(fn(node_id) {
      let neighbors = dict.get(neighbor_map, node_id) |> result.unwrap([])

      let initial_state =
        PushSumState(
          node_id: node_id,
          neighbors: neighbors,
          neighbor_subjects: dict.new(),
          s: int.to_float(node_id + 1),
          // 1..N
          w: 1.0,
          previous_ratios: [],
          is_active: True,
          consecutive_stable_count: 0,
          convergence_threshold: 1.0e-10,
          stability_rounds: 3,
          rng: node_id * 65_537 + 9731,
        )

      case
        actor.new(initial_state)
        |> actor.on_message(handle_push_sum_message)
        |> actor.start
      {
        Ok(actor) -> Ok(#(node_id, actor.data))
        Error(_) -> Error(#(node_id, "Failed to start actor"))
      }
    })

  let subjects = list.try_map(actor_results, fn(result) { result })
  case subjects {
    Ok(subject_list) -> {
      let subject_dict = dict.from_list(subject_list)

      list.each(subject_list, fn(pair) {
        let #(_, subject) = pair
        process.send(subject, UpdateNeighborSubjects(subject_dict))
      })
      process.sleep(10)
      Ok(subject_dict)
    }
    Error(_) -> Error("Failed to start push-sum actors")
  }
}

fn handle_push_sum_message(
  state: PushSumState,
  message: PushSumMessage,
) -> actor.Next(PushSumState, PushSumMessage) {
  case message {
    UpdateNeighborSubjects(subjects) -> {
      let neighbor_subjects =
        list.filter_map(state.neighbors, fn(neighbor_id) {
          case dict.get(subjects, neighbor_id) {
            Ok(subject) -> Ok(#(neighbor_id, subject))
            Error(_) -> Error(Nil)
          }
        })
        |> dict.from_list

      actor.continue(
        PushSumState(..state, neighbor_subjects: neighbor_subjects),
      )
    }

    StartPushSum -> actor.continue(state)

    Tick -> {
      case state.is_active && state.neighbors != [] {
        False -> actor.continue(state)
        True -> {
          // One asynchronous "round": send half, keep half
          let out_s = state.s /. 2.0
          let out_w = state.w /. 2.0
          let st1 = PushSumState(..state, s: out_s, w: out_w)

          let n = list.length(st1.neighbors)
          let #(idx, rng2) = pick_index(n, st1.rng)
          case list.drop(st1.neighbors, idx) |> list.first {
            Ok(neighbor_id) -> {
              case dict.get(st1.neighbor_subjects, neighbor_id) {
                Ok(neighbor_subject) ->
                  process.send(neighbor_subject, PushSumPair(out_s, out_w))
                Error(_) -> Nil
              }
              actor.continue(PushSumState(..st1, rng: rng2))
            }
            Error(_) -> actor.continue(PushSumState(..st1, rng: rng2))
          }
        }
      }
    }

    PushSumPair(received_s, received_w) -> {
      case state.is_active {
        False -> actor.continue(state)
        True -> {
          // Combine; ratio for stability check
          let combined_s = state.s +. received_s
          let combined_w = state.w +. received_w
          let new_ratio = case combined_w == 0.0 {
            True -> 0.0
            False -> combined_s /. combined_w
          }

          let updated_ratios =
            [new_ratio, ..state.previous_ratios]
            |> list.take(state.stability_rounds)

          let is_stable =
            check_ratio_stability(updated_ratios, state.convergence_threshold)
          let new_consecutive_stable = case is_stable {
            True -> state.consecutive_stable_count + 1
            False -> 0
          }
          let should_stop = new_consecutive_stable >= state.stability_rounds

          // Keep the combined values; next Tick will split and send
          let st1 =
            PushSumState(
              ..state,
              s: combined_s,
              w: combined_w,
              previous_ratios: updated_ratios,
              consecutive_stable_count: new_consecutive_stable,
              is_active: !should_stop,
            )

          actor.continue(st1)
        }
      }
    }

    CheckConvergence(reply_to) -> {
      let converged =
        !state.is_active
        && state.consecutive_stable_count >= state.stability_rounds
      process.send(reply_to, converged)
      actor.continue(state)
    }

    GetStatus(reply_to) -> {
      let current_ratio = case state.w == 0.0 {
        True -> 0.0
        False -> state.s /. state.w
      }
      process.send(
        reply_to,
        PushSumStatus(
          state.node_id,
          state.s,
          state.w,
          current_ratio,
          state.is_active,
          state.consecutive_stable_count,
        ),
      )
      actor.continue(state)
    }

    Shutdown -> actor.stop()
  }
}

fn check_ratio_stability(ratios: List(Float), threshold: Float) -> Bool {
  case ratios {
    [r0, r1, r2, ..] ->
      float.absolute_value(r0 -. r1) <=. threshold
      && float.absolute_value(r1 -. r2) <=. threshold
      && float.absolute_value(r0 -. r2) <=. threshold
    _ -> False
  }
}

fn broadcast_tick_push(actors: Dict(Int, Subject(PushSumMessage))) -> Nil {
  dict.each(actors, fn(_, subject) { process.send(subject, Tick) })
}

fn wait_for_push_sum_convergence(
  actors: Dict(Int, Subject(PushSumMessage)),
) -> Int {
  wait_for_push_sum_convergence_helper(actors, 0)
}

fn wait_for_push_sum_convergence_helper(
  actors: Dict(Int, Subject(PushSumMessage)),
  attempt: Int,
) -> Int {
  broadcast_tick_push(actors)
  process.sleep(10)
  let converged = check_all_push_sum_converged(actors)
  case converged || attempt > 20_000 {
    True -> attempt * 10
    False -> wait_for_push_sum_convergence_helper(actors, attempt + 1)
  }
}

fn check_all_push_sum_converged(
  actors: Dict(Int, Subject(PushSumMessage)),
) -> Bool {
  check_push_sum_convergence_for_actors(dict.to_list(actors))
}

fn check_push_sum_convergence_for_actors(
  actors: List(#(Int, Subject(PushSumMessage))),
) -> Bool {
  case actors {
    [] -> True
    [#(_, subject), ..rest] -> {
      let reply_subject = process.new_subject()
      process.send(subject, CheckConvergence(reply_subject))
      case process.receive(reply_subject, 100) {
        Ok(True) -> check_push_sum_convergence_for_actors(rest)
        _ -> False
      }
    }
  }
}

fn shutdown_push_sum_actors(actors: Dict(Int, Subject(PushSumMessage))) -> Nil {
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
