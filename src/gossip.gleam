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

pub type GossipMessage {
  Rumor(content: String)
  StartGossip(rumor: String)
  UpdateNeighborSubjects(
    subjects: Dict(Int, Subject(GossipMessage)),
    reply_to: Subject(Nil),
  )
  Tick
  CheckConvergence(reply_to: Subject(Bool))
  GetStatus(reply_to: Subject(GossipStatus))
  Shutdown
}

pub type GossipStatus {
  GossipStatus(node_id: Int, rumor_count: Int, is_active: Bool)
}

pub type GossipState {
  GossipState(
    node_id: Int,
    neighbors: List(Int),
    neighbor_subjects: Dict(Int, Subject(GossipMessage)),
    rumor: String,
    rumor_count: Int,
    is_active: Bool,
    convergence_threshold: Int,
    rng: Int,
    send_count: Int,
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

pub fn run_gossip_simulation(
  num_nodes: Int,
  neighbor_map: NeighborMap,
  failure_rate: Float,
) -> Result(Int, String) {
  io.println(
    "Starting gossip simulation with " <> int.to_string(num_nodes) <> " nodes",
  )
  print()
  let start_ms = now_milliseconds()
  use actors <- result.try(start_gossip_actors(
    num_nodes,
    neighbor_map,
    failure_rate,
  ))
  io.println("Gossip actors started successfully")

  case dict.get(actors, 0) {
    Ok(first_actor) -> {
      process.send(first_actor, StartGossip("Hello, this is a rumor!"))
      io.println("Waiting for gossip convergence")
      // Wait for convergence; ignore tick-based count
      let _ticks = wait_for_gossip_convergence(actors)
      let end_ms = now_milliseconds()
      let elapsed = elapsed_ms(start_ms, end_ms)
      io.println("Gossip elapsed wall time " <> int.to_string(elapsed) <> " ms")
      shutdown_gossip_actors(actors)
      io.println("Gossip actors shut down")
      Ok(elapsed)
    }
    Error(_) -> Error("Failed to start gossip simulation")
  }
}

fn start_gossip_actors(
  num_nodes: Int,
  neighbor_map: NeighborMap,
  failure_rate: Float,
) -> Result(Dict(Int, Subject(GossipMessage)), String) {
  let actor_results =
    list.range(0, num_nodes - 1)
    |> list.map(fn(node_id) {
      let neighbors = dict.get(neighbor_map, node_id) |> result.unwrap([])

      let initial_state =
        GossipState(
          node_id: node_id,
          neighbors: neighbors,
          neighbor_subjects: dict.new(),
          rumor: "",
          rumor_count: 0,
          is_active: True,
          convergence_threshold: 10,
          rng: node_id * 48_271 + 12_345,
          send_count: 0,
          failure_rate: failure_rate,
        )

      case
        actor.new(initial_state)
        |> actor.on_message(handle_gossip_message)
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
        process.receive(ready_subject, 5000)
        |> result.unwrap(Nil)
      })

      Ok(subject_dict)
    }
    Error(_) -> Error("Failed to start gossip actors")
  }
}

fn handle_gossip_message(
  state: GossipState,
  message: GossipMessage,
) -> actor.Next(GossipState, GossipMessage) {
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

      actor.continue(GossipState(..state, neighbor_subjects: neighbor_subjects))
    }

    StartGossip(rumor) -> {
      // Mark as having heard once; periodic Tick will keep sending
      let st1 =
        GossipState(..state, rumor: rumor, rumor_count: 1, send_count: 0)
      actor.continue(st1)
    }

    Tick -> {
      // Periodic push while active and rumor known
      case state.is_active && state.rumor != "" && state.neighbors != [] {
        False -> {
          actor.continue(state)
        }
        True -> {
          let n = list.length(state.neighbors)
          let #(idx, rng2) = pick_index(n, state.rng)
          case list.drop(state.neighbors, idx) |> list.first {
            Ok(neighbor_id) -> {
              let threshold = float.truncate(state.failure_rate *. 100.0)
              let rand = next_rng(rng2) % 100
              let rng3 = next_rng(rand)
              case rand < threshold {
                True -> {
                  // Message failed
                  io.println(
                    "Node "
                    <> int.to_string(state.node_id)
                    <> " gossip message to node "
                    <> int.to_string(neighbor_id)
                    <> " failed (simulated)",
                  )
                  actor.continue(GossipState(..state, rng: rng3))
                }
                False -> {
                  io.println(
                    "Node "
                    <> int.to_string(state.node_id)
                    <> " sending rumor to node "
                    <> int.to_string(neighbor_id),
                  )
                  case dict.get(state.neighbor_subjects, neighbor_id) {
                    Ok(neighbor_subject) ->
                      process.send(neighbor_subject, Rumor(state.rumor))
                    Error(_) -> Nil
                  }
                  let new_send_count = state.send_count + 1
                  let new_active = new_send_count < state.convergence_threshold
                  actor.continue(
                    GossipState(
                      ..state,
                      rng: rng3,
                      send_count: new_send_count,
                      is_active: new_active,
                    ),
                  )
                }
              }
            }
            Error(_) -> {
              actor.continue(GossipState(..state, rng: rng2))
            }
          }
        }
      }
    }

    Rumor(content) -> {
      case content == state.rumor {
        False -> {
          // First time for this rumor
          let st1 = GossipState(..state, rumor: content, rumor_count: 1)
          actor.continue(st1)
        }
        True -> {
          let new_count = state.rumor_count + 1
          let st1 = GossipState(..state, rumor_count: new_count)
          actor.continue(st1)
        }
      }
    }

    CheckConvergence(reply_to) -> {
      let converged = state.rumor != ""
      // io.println(
      //   "Node "
      //   <> int.to_string(state.node_id)
      //   <> " convergence check: has_rumor="
      //   <> bool_to_string(converged),
      // )
      process.send(reply_to, converged)
      actor.continue(state)
    }

    GetStatus(reply_to) -> {
      process.send(
        reply_to,
        GossipStatus(state.node_id, state.rumor_count, state.is_active),
      )
      actor.continue(state)
    }

    Shutdown -> {
      // io.println("Node " <> int.to_string(state.node_id) <> " shutting down")
      actor.stop()
    }
  }
}

fn broadcast_tick_gossip(actors: Dict(Int, Subject(GossipMessage))) -> Nil {
  dict.each(actors, fn(_, subject) { process.send(subject, Tick) })
}

fn wait_for_gossip_convergence(actors: Dict(Int, Subject(GossipMessage))) -> Int {
  wait_for_convergence_helper(actors, 0)
}

fn wait_for_convergence_helper(
  actors: Dict(Int, Subject(GossipMessage)),
  attempt: Int,
) -> Int {
  broadcast_tick_gossip(actors)
  process.sleep(10)
  let converged = check_all_converged(actors)
  // io.println("Convergence status: " <> bool_to_string(converged))
  case converged || attempt > 100{
    True -> {
      let ticks = attempt
      io.println("Convergence reached in ~" <> int.to_string(ticks) <> " ticks")
      ticks * 10
    }
    False -> wait_for_convergence_helper(actors, attempt + 1)
  }
}

fn check_all_converged(actors: Dict(Int, Subject(GossipMessage))) -> Bool {
  check_convergence_for_actors(dict.to_list(actors))
}

fn check_convergence_for_actors(
  actors: List(#(Int, Subject(GossipMessage))),
) -> Bool {
  case actors {
    [] -> True
    [#(_, subject), ..rest] -> {
      let reply_subject = process.new_subject()
      process.send(subject, CheckConvergence(reply_subject))
      case process.receive(reply_subject, 100) {
        Ok(True) -> check_convergence_for_actors(rest)
        _ -> False
      }
    }
  }
}

fn shutdown_gossip_actors(actors: Dict(Int, Subject(GossipMessage))) -> Nil {
  io.println("Shutting down gossip actors")
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
