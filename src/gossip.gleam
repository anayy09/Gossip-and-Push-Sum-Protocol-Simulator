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
  )
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

      // Small delay to ensure UpdateNeighborSubjects is processed before StartGossip
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

      let new_state = GossipState(..state, neighbor_subjects: neighbor_subjects)
      actor.continue(new_state)
    }

    StartGossip(rumor) -> {
      let new_state = GossipState(..state, rumor: rumor, rumor_count: 1)
      spread_rumor(new_state)
      actor.continue(new_state)
    }

    Rumor(content) -> {
      case state.is_active {
        False -> actor.continue(state)
        True -> {
          case content == state.rumor {
            False -> {
              let new_state =
                GossipState(..state, rumor: content, rumor_count: 1)
              spread_rumor(new_state)
              actor.continue(new_state)
            }
            True -> {
              let new_count = state.rumor_count + 1
              let is_still_active = new_count < state.convergence_threshold

              let new_state =
                GossipState(
                  ..state,
                  rumor_count: new_count,
                  is_active: is_still_active,
                )

              case is_still_active {
                True -> spread_rumor(new_state)
                False -> Nil
              }

              actor.continue(new_state)
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
      let status =
        GossipStatus(
          node_id: state.node_id,
          rumor_count: state.rumor_count,
          is_active: state.is_active,
        )
      process.send(reply_to, status)
      actor.continue(state)
    }

    Shutdown -> actor.stop()
  }
}

fn spread_rumor(state: GossipState) -> Nil {
  case state.is_active && state.neighbors != [] {
    False -> Nil
    True -> {
      let neighbor_count = list.length(state.neighbors)
      let seed =
        { state.node_id * 1_103_515_245 + state.rumor_count * 12_345 }
        % 2_147_483_647
      let random_index = { int.absolute_value(seed) } % neighbor_count

      case list.drop(state.neighbors, random_index) |> list.first {
        Ok(neighbor_id) -> {
          case dict.get(state.neighbor_subjects, neighbor_id) {
            Ok(neighbor_subject) -> {
              process.send(neighbor_subject, Rumor(state.rumor))
            }
            Error(_) -> Nil
          }
        }
        Error(_) -> Nil
      }
    }
  }
}

fn wait_for_gossip_convergence(actors: Dict(Int, Subject(GossipMessage))) -> Int {
  wait_for_convergence_helper(actors, 0)
}

fn wait_for_convergence_helper(actors, attempt) -> Int {
  process.sleep(10)
  let converged = check_all_converged(actors)
  case converged || attempt > 20_000 {
    True -> attempt * 10
    False -> wait_for_convergence_helper(actors, attempt + 1)
  }
}

fn check_all_converged(actors: Dict(Int, Subject(GossipMessage))) -> Bool {
  let actor_list = dict.to_list(actors)
  check_convergence_for_actors(actor_list)
}

fn check_convergence_for_actors(
  actors: List(#(Int, Subject(GossipMessage))),
) -> Bool {
  case actors {
    [] -> True
    [#(_, subject), ..rest] -> {
      let reply_subject = process.new_subject()
      process.send(subject, CheckConvergence(reply_subject))

      case process.receive(reply_subject, 1000) {
        Ok(True) -> check_convergence_for_actors(rest)
        _ -> False
      }
    }
  }
}

fn shutdown_gossip_actors(actors: Dict(Int, Subject(GossipMessage))) -> Nil {
  dict.each(actors, fn(_, subject) { process.send(subject, Shutdown) })
}
