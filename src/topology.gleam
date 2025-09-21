import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub type Topology {
  Full
  ThreeDGrid
  Line
  ImperfectThreeDGrid
}

pub type NeighborMap =
  Dict(Int, List(Int))

pub fn create_topology(
  num_nodes: Int,
  topology: Topology,
) -> Result(NeighborMap, String) {
  io.println(
    "Creating topology: "
    <> topology_to_string(topology)
    <> " with "
    <> int.to_string(num_nodes)
    <> " nodes",
  )
  let result = case topology {
    Full -> create_full_topology(num_nodes)
    ThreeDGrid -> create_3d_grid_topology(num_nodes)
    Line -> create_line_topology(num_nodes)
    ImperfectThreeDGrid -> create_imperfect_3d_grid_topology(num_nodes)
  }
  case result {
    Ok(_) -> io.println("Topology created successfully")
    Error(_) -> Nil
  }
  result
}

fn topology_to_string(topo: Topology) -> String {
  case topo {
    Full -> "full"
    ThreeDGrid -> "3D"
    Line -> "line"
    ImperfectThreeDGrid -> "imp3D"
  }
}

fn create_full_topology(num_nodes: Int) -> Result(NeighborMap, String) {
  let node_ids = list.range(0, num_nodes - 1)
  let neighbor_map =
    list.map(node_ids, fn(node_id) {
      let neighbors = list.filter(node_ids, fn(n) { n != node_id })
      #(node_id, neighbors)
    })
    |> dict.from_list

  Ok(neighbor_map)
}

fn create_line_topology(num_nodes: Int) -> Result(NeighborMap, String) {
  let neighbor_map =
    list.range(0, num_nodes - 1)
    |> list.map(fn(node_id) {
      let neighbors = case node_id {
        0 ->
          case num_nodes > 1 {
            True -> [1]
            False -> []
          }
        n if n == num_nodes - 1 -> [n - 1]
        n -> [n - 1, n + 1]
      }
      #(node_id, neighbors)
    })
    |> dict.from_list

  Ok(neighbor_map)
}

fn create_3d_grid_topology(num_nodes: Int) -> Result(NeighborMap, String) {
  let cube_size = find_cube_size(num_nodes)

  let neighbor_map =
    list.range(0, num_nodes - 1)
    |> list.map(fn(node_id) {
      let #(x, y, z) = node_id_to_3d_coords(node_id, cube_size)
      let neighbors = get_3d_grid_neighbors(x, y, z, cube_size, num_nodes)
      #(node_id, neighbors)
    })
    |> dict.from_list

  Ok(neighbor_map)
}

fn create_imperfect_3d_grid_topology(
  num_nodes: Int,
) -> Result(NeighborMap, String) {
  use base_topology <- result.try(create_3d_grid_topology(num_nodes))

  let enhanced_topology =
    dict.map_values(base_topology, fn(node_id, base_neighbors) {
      let random_neighbor =
        generate_random_neighbor(node_id, base_neighbors, num_nodes)
      case list.contains(base_neighbors, random_neighbor) {
        True -> base_neighbors
        False -> [random_neighbor, ..base_neighbors]
      }
    })

  Ok(enhanced_topology)
}

fn find_cube_size(num_nodes: Int) -> Int {
  find_cube_size_helper(1, num_nodes)
}

fn find_cube_size_helper(size: Int, num_nodes: Int) -> Int {
  case size * size * size >= num_nodes {
    True -> size
    False -> find_cube_size_helper(size + 1, num_nodes)
  }
}

fn node_id_to_3d_coords(node_id: Int, cube_size: Int) -> #(Int, Int, Int) {
  let z = node_id / { cube_size * cube_size }
  let y = { node_id % { cube_size * cube_size } } / cube_size
  let x = node_id % cube_size
  #(x, y, z)
}

fn coords_3d_to_node_id(x: Int, y: Int, z: Int, cube_size: Int) -> Int {
  z * cube_size * cube_size + y * cube_size + x
}

fn get_3d_grid_neighbors(
  x: Int,
  y: Int,
  z: Int,
  cube_size: Int,
  num_nodes: Int,
) -> List(Int) {
  let directions = [
    #(-1, 0, 0),
    #(1, 0, 0),
    // x-direction
    #(0, -1, 0),
    #(0, 1, 0),
    // y-direction  
    #(0, 0, -1),
    #(0, 0, 1),
    // z-direction
  ]

  directions
  |> list.filter_map(fn(direction) {
    let #(dx, dy, dz) = direction
    let new_x = x + dx
    let new_y = y + dy
    let new_z = z + dz

    case
      new_x >= 0
      && new_x < cube_size
      && new_y >= 0
      && new_y < cube_size
      && new_z >= 0
      && new_z < cube_size
    {
      True -> {
        let neighbor_id = coords_3d_to_node_id(new_x, new_y, new_z, cube_size)
        case neighbor_id < num_nodes {
          True -> Ok(neighbor_id)
          False -> Error(Nil)
        }
      }
      False -> Error(Nil)
    }
  })
}

fn generate_random_neighbor(
  node_id: Int,
  existing_neighbors: List(Int),
  num_nodes: Int,
) -> Int {
  let seed = { node_id * 1_103_515_245 + 12_345 } % 2_147_483_647
  let random_neighbor = seed % num_nodes

  case
    random_neighbor == node_id
    || list.contains(existing_neighbors, random_neighbor)
  {
    True -> { random_neighbor + 1 } % num_nodes
    False -> random_neighbor
  }
}
