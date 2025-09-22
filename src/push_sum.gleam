import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/otp/actor
import gleam/result
import topology.{type NeighborMap}
import utils.{elapsed_ms, now_milliseconds, print}

// fn bool_to_string(b: Bool) -> String {
//   case b {
//     True -> "true"
//     False -> "false"
//   }
// }

pub type PushSumMessage {
  PushSumPair(s: Float, w: Float)
  StartPushSum
  UpdateNeighborSubjects(
    subjects: Dict(Int, Subject(PushSumMessage)),
    reply_to: Subject(Nil),
  )
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
    failure_rate: Float,
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
  failure_rate: Float,
) -> Result(Int, String) {
  io.println(
    "Starting push-sum simulation with " <> int.to_string(num_nodes) <> " nodes",
  )
  print()
  let start_ms = now_milliseconds()
  use actors <- result.try(start_push_sum_actors(
    num_nodes,
    neighbor_map,
    failure_rate,
  ))
  io.println("Push-sum actors started successfully")

  case dict.get(actors, 0) {
    Ok(first_actor) -> {
      process.send(first_actor, StartPushSum)
      io.println("Waiting for push-sum convergence")
      let time_taken = wait_for_push_sum_convergence(actors)
      let end_ms = now_milliseconds()
      let elapsed = elapsed_ms(start_ms, end_ms)
      io.println(
        "Push-sum converged in ~" <> int.to_string(time_taken) <> " ticks",
      )
      io.println(
        "Push-sum elapsed wall time " <> int.to_string(elapsed) <> " ms",
      )
      shutdown_push_sum_actors(actors)
      io.println("Push-sum actors shut down")
      Ok(elapsed)
    }
    Error(_) -> Error("Failed to start push-sum simulation")
  }
}

fn start_push_sum_actors(
  num_nodes: Int,
  neighbor_map: NeighborMap,
  failure_rate: Float,
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
          failure_rate: failure_rate,
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

      // Create a subject for actors to reply to
      let ready_subject = process.new_subject()

      // Send the setup message to all actors with the reply_to address
      list.each(subject_list, fn(pair) {
        let #(_, subject) = pair
        process.send(
          subject,
          UpdateNeighborSubjects(subject_dict, reply_to: ready_subject),
        )
      })

      // Wait for all actors to confirm they are ready
      list.range(0, num_nodes - 1)
      |> list.each(fn(_) {
        // This will block until a message is received
        process.receive(ready_subject, 5000)
        |> result.unwrap(Nil)
      })

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
    UpdateNeighborSubjects(subjects, reply_to) -> {
      let neighbor_subjects =
        list.filter_map(state.neighbors, fn(neighbor_id) {
          case dict.get(subjects, neighbor_id) {
            Ok(subject) -> Ok(#(neighbor_id, subject))
            Error(_) -> Error(Nil)
          }
        })
        |> dict.from_list

      // Send the confirmation message
      process.send(reply_to, Nil)

      actor.continue(
        PushSumState(..state, neighbor_subjects: neighbor_subjects),
      )
    }

    StartPushSum -> {
      actor.continue(state)
    }

    Tick -> {
      case state.is_active && state.neighbors != [] {
        False -> {
          actor.continue(state)
        }
        True -> {
          // One asynchronous "round": send half, keep half
          let out_s = state.s /. 2.0
          let out_w = state.w /. 2.0
          let new_ratio = case out_w == 0.0 {
            True -> 0.0
            False -> out_s /. out_w
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

          let st1 =
            PushSumState(
              ..state,
              s: out_s,
              w: out_w,
              previous_ratios: updated_ratios,
              consecutive_stable_count: new_consecutive_stable,
              is_active: new_consecutive_stable < state.stability_rounds,
            )

          let n = list.length(st1.neighbors)
          let #(idx, rng2) = pick_index(n, st1.rng)
          case list.drop(st1.neighbors, idx) |> list.first {
            Ok(neighbor_id) -> {
              let threshold = float.truncate(st1.failure_rate *. 100.0)
              let rand = next_rng(rng2) % 100
              let rng3 = next_rng(rand)
              case rand < threshold {
                True -> {
                  // Message failed
                  io.println(
                    "Node "
                    <> int.to_string(st1.node_id)
                    <> " push-sum message to node "
                    <> int.to_string(neighbor_id)
                    <> " failed (simulated)",
                  )
                  actor.continue(PushSumState(..st1, rng: rng3))
                }
                False -> {
                  io.println(
                    "Node "
                    <> int.to_string(st1.node_id)
                    <> " sending push-sum (s="
                    <> float.to_string(out_s)
                    <> ", w="
                    <> float.to_string(out_w)
                    <> ", ratio="
                    <> float.to_string(new_ratio)
                    <> ") to node "
                    <> int.to_string(neighbor_id),
                  )
                  case dict.get(st1.neighbor_subjects, neighbor_id) {
                    Ok(neighbor_subject) ->
                      process.send(neighbor_subject, PushSumPair(out_s, out_w))
                    Error(_) -> Nil
                  }
                  actor.continue(PushSumState(..st1, rng: rng3))
                }
              }
            }
            Error(_) -> {
              actor.continue(PushSumState(..st1, rng: rng2))
            }
          }
        }
      }
    }

    PushSumPair(received_s, received_w) -> {
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

      // Keep the combined values; next Tick will split and send
      let st1 =
        PushSumState(
          ..state,
          s: combined_s,
          w: combined_w,
          previous_ratios: updated_ratios,
          consecutive_stable_count: new_consecutive_stable,
          is_active: new_consecutive_stable < state.stability_rounds,
        )

      actor.continue(st1)
    }

    CheckConvergence(reply_to) -> {
      let converged = state.consecutive_stable_count >= state.stability_rounds
      // io.println(
      //   "Node "
      //   <> int.to_string(state.node_id)
      //   <> " convergence check: consecutive="
      //   <> int.to_string(state.consecutive_stable_count)
      //   <> ", threshold="
      //   <> int.to_string(state.stability_rounds)
      //   <> ", converged="
      //   <> bool_to_string(converged),
      // )
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

    Shutdown -> {
      // io.println("Node " <> int.to_string(state.node_id) <> " shutting down")
      actor.stop()
    }
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
  case converged || attempt > 100 {
    True -> {
      let ticks = attempt
      io.println(
        "Push-sum convergence reached in ~" <> int.to_string(ticks) <> " ticks",
      )
      ticks * 10
    }
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
        Ok(True) -> {
          check_push_sum_convergence_for_actors(rest)
        }
        _ -> False
      }
    }
  }
}

fn shutdown_push_sum_actors(actors: Dict(Int, Subject(PushSumMessage))) -> Nil {
  io.println("Shutting down push-sum actors")
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
