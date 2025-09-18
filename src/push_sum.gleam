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
  )
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
          w: 1.0,
          previous_ratios: [],
          is_active: True,
          consecutive_stable_count: 0,
          convergence_threshold: 1.0e-10,
          stability_rounds: 3,
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

      // Small delay to ensure UpdateNeighborSubjects is processed before StartPushSum
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

      let new_state =
        PushSumState(..state, neighbor_subjects: neighbor_subjects)
      actor.continue(new_state)
    }

    StartPushSum -> {
      send_push_sum_message(state)
      actor.continue(state)
    }

    PushSumPair(received_s, received_w) -> {
      case state.is_active {
        False -> actor.continue(state)
        True -> {
          let new_s = state.s +. received_s
          let new_w = state.w +. received_w

          let new_ratio = case new_w == 0.0 {
            True -> 0.0
            False -> new_s /. new_w
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

          let is_still_active = new_consecutive_stable < state.stability_rounds

          let new_state =
            PushSumState(
              ..state,
              s: new_s /. 2.0,
              w: new_w /. 2.0,
              previous_ratios: updated_ratios,
              consecutive_stable_count: new_consecutive_stable,
              is_active: is_still_active,
            )

          case is_still_active {
            True -> send_push_sum_message(new_state)
            False -> Nil
          }

          actor.continue(new_state)
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

      let status =
        PushSumStatus(
          node_id: state.node_id,
          s: state.s,
          w: state.w,
          ratio: current_ratio,
          is_active: state.is_active,
          consecutive_stable: state.consecutive_stable_count,
        )
      process.send(reply_to, status)
      actor.continue(state)
    }

    Shutdown -> actor.stop()
  }
}

fn send_push_sum_message(state: PushSumState) -> Nil {
  case state.is_active && state.neighbors != [] {
    False -> Nil
    True -> {
      let neighbor_count = list.length(state.neighbors)
      let seed =
        float.truncate(state.s *. 1000.0) + state.consecutive_stable_count
      let random_index =
        { { seed * 1_103_515_245 + 12_345 } |> int.absolute_value }
        % neighbor_count

      case list.drop(state.neighbors, random_index) |> list.first {
        Ok(neighbor_id) -> {
          case dict.get(state.neighbor_subjects, neighbor_id) {
            Ok(neighbor_subject) -> {
              process.send(
                neighbor_subject,
                PushSumPair(state.s /. 2.0, state.w /. 2.0),
              )
            }
            Error(_) -> Nil
          }
        }
        Error(_) -> Nil
      }
    }
  }
}

fn check_ratio_stability(ratios: List(Float), threshold: Float) -> Bool {
  case ratios {
    [current, ..rest] -> {
      case rest {
        [] -> False
        _ -> {
          list.all(rest, fn(previous_ratio) {
            float.absolute_value(current -. previous_ratio) <=. threshold
          })
        }
      }
    }
    [] -> False
  }
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
  process.sleep(100)

  let converged = check_all_push_sum_converged(actors)
  case converged || attempt > 100 {
    True -> attempt * 100
    False -> wait_for_push_sum_convergence_helper(actors, attempt + 1)
  }
}

fn check_all_push_sum_converged(
  actors: Dict(Int, Subject(PushSumMessage)),
) -> Bool {
  let actor_list = dict.to_list(actors)
  check_push_sum_convergence_for_actors(actor_list)
}

fn check_push_sum_convergence_for_actors(
  actors: List(#(Int, Subject(PushSumMessage))),
) -> Bool {
  case actors {
    [] -> True
    [#(_, subject), ..rest] -> {
      let reply_subject = process.new_subject()
      process.send(subject, CheckConvergence(reply_subject))

      case process.receive(reply_subject, 1000) {
        Ok(True) -> check_push_sum_convergence_for_actors(rest)
        _ -> False
      }
    }
  }
}

fn shutdown_push_sum_actors(actors: Dict(Int, Subject(PushSumMessage))) -> Nil {
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
