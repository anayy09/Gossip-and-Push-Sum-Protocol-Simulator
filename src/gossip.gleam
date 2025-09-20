import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/result
import topology.{type NeighborMap}

pub type GossipMessage {
  Rumor(content: String)
  StartGossip(rumor: String)
  UpdateNeighborSubjects(subjects: Dict(Int, Subject(GossipMessage)))
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
  )
}

const mod_ = 2_147_483_647

fn next_rng(r: Int) -> Int {
  let v = {r * 1_103_515_245 + 12_345} % mod_
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
) -> Result(Int, String) {
  use actors <- result.try(start_gossip_actors(num_nodes, neighbor_map))

  let rumor = "Hello, this is a rumor!"
  case dict.get(actors, 0) {
    Ok(first_actor) -> {
      process.send(first_actor, StartGossip(rumor))
      let time_taken = wait_for_gossip_convergence(actors)
      shutdown_gossip_actors(actors)
      Ok(time_taken)
    }
    Error(_) -> Error("Failed to start gossip simulation")
  }
}

fn start_gossip_actors(
  num_nodes: Int,
  neighbor_map: NeighborMap,
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

      list.each(subject_list, fn(pair) {
        let #(_, subject) = pair
        process.send(subject, UpdateNeighborSubjects(subject_dict))
      })
      process.sleep(10)
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
    UpdateNeighborSubjects(subjects) -> {
      let neighbor_subjects =
        list.filter_map(state.neighbors, fn(neighbor_id) {
          case dict.get(subjects, neighbor_id) {
            Ok(subject) -> Ok(#(neighbor_id, subject))
            Error(_) -> Error(Nil)
          }
        })
        |> dict.from_list

      actor.continue(GossipState(..state, neighbor_subjects: neighbor_subjects))
    }

    StartGossip(rumor) -> {
      // Mark as having heard once; periodic Tick will keep sending
      let st1 = GossipState(..state, rumor: rumor, rumor_count: 1)
      actor.continue(st1)
    }

    Tick -> {
      // Periodic push while active and rumor known
      case state.is_active && state.rumor != "" && state.neighbors != [] {
        False -> actor.continue(state)
        True -> {
          let n = list.length(state.neighbors)
          let #(idx, rng2) = pick_index(n, state.rng)
          case list.drop(state.neighbors, idx) |> list.first {
            Ok(neighbor_id) -> {
              case dict.get(state.neighbor_subjects, neighbor_id) {
                Ok(neighbor_subject) -> process.send(neighbor_subject, Rumor(state.rumor))
                Error(_) -> Nil
              }
              actor.continue(GossipState(..state, rng: rng2))
            }
            Error(_) -> actor.continue(GossipState(..state, rng: rng2))
          }
        }
      }
    }

    Rumor(content) -> {
      case state.is_active {
        False -> actor.continue(state)
        True -> {
          case content == state.rumor {
            False -> {
              // First time for this rumor
              let st1 = GossipState(..state, rumor: content, rumor_count: 1)
              actor.continue(st1)
            }
            True -> {
              let new_count = state.rumor_count + 1
              let st1 =
                GossipState(
                  ..state,
                  rumor_count: new_count,
                  is_active: new_count < state.convergence_threshold,
                )
              actor.continue(st1)
            }
          }
        }
      }
    }

    CheckConvergence(reply_to) -> {
      process.send(reply_to, state.rumor_count >= state.convergence_threshold)
      actor.continue(state)
    }

    GetStatus(reply_to) -> {
      process.send(
        reply_to,
        GossipStatus(state.node_id, state.rumor_count, state.is_active),
      )
      actor.continue(state)
    }

    Shutdown -> actor.stop()
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
  case converged || attempt > 20_000 {
    True -> attempt * 10
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
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
