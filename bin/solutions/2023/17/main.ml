open Core
open Util
open Pairing_heap

let parse_digit_grid input =
  String.split_lines input
  |> List.map ~f:String.to_array
  |> List.map ~f:(Array.map ~f:Char.get_digit_exn)
  |> Array.of_list
;;

module State = struct
  module T = struct
    type t =
      { r : int
      ; c : int
      ; dr : int
      ; dc : int
      ; steps_in_dir : int
      }
    [@@deriving compare, sexp_of, hash]

    let start = { r = 0; c = 0; dr = 0; dc = 0; steps_in_dir = 0 }
    let has_dir state dr dc = state.dr = dr && state.dc = dc

    let get_successors ?min_steps ~max_steps grid state dist =
      let rows = Array.length grid in
      let cols = Array.length grid.(0) in
      let directions = [ 0, 1; 0, -1; 1, 0; -1, 0 ] in
      List.filter_map directions ~f:(fun (dr, dc) ->
        let r = state.r + dr in
        let c = state.c + dc in
        let same_dir = has_dir state dr dc in
        if r < 0 || r >= rows || c < 0 || c >= cols
        then None
        else if has_dir state (-dr) (-dc)
        then None
        else if same_dir && state.steps_in_dir >= max_steps
        then None
        else if state.steps_in_dir > 0
                && (not same_dir)
                && Option.exists min_steps ~f:(fun min_steps ->
                  state.steps_in_dir < min_steps)
        then None
        else (
          let steps_in_dir = if same_dir then state.steps_in_dir + 1 else 1 in
          Some (dist + grid.(r).(c), { r; c; dr; dc; steps_in_dir })))
    ;;

    let is_goal ?min_steps grid state =
      let rows = Array.length grid in
      let cols = Array.length grid.(0) in
      state.r = rows - 1
      && state.c = cols - 1
      && Option.for_all min_steps ~f:(fun min_steps ->
        state.steps_in_dir >= min_steps)
    ;;
  end

  include T
  include Comparator.Make (T)
end

let find_shortest_path grid ~is_goal ~get_succesors =
  let compare (d1, _) (d2, _) = Int.compare d1 d2 in
  let rec loop heap visited =
    let (dist, state), heap = Heap.pop_exn heap in
    if is_goal state
    then dist
    else if Set.mem visited state
    then loop heap visited
    else (
      let visited = Set.add visited state in
      let heap = Heap.insert_list heap (get_succesors state dist) in
      loop heap visited)
  in
  let heap = Heap.singleton compare (0, State.start) in
  let visited = Set.empty (module State) in
  loop heap visited
;;

let solve_1 input =
  let grid = parse_digit_grid input in
  let get_succesors = State.get_successors ~max_steps:3 grid in
  let is_goal = State.is_goal grid in
  find_shortest_path grid ~is_goal ~get_succesors |> Int.to_string
;;

let solve_2 input =
  let grid = parse_digit_grid input in
  let get_succesors = State.get_successors ~min_steps:4 ~max_steps:10 grid in
  let is_goal = State.is_goal ~min_steps:4 grid in
  find_shortest_path grid ~is_goal ~get_succesors |> Int.to_string
;;

Printing.output_solution
  ~part:1
  ~solve:solve_1
  ~input:Input.text
  ~example_input:Example_input.text
;;

Printing.output_solution
  ~part:2
  ~solve:solve_2
  ~input:Input.text
  ~example_input:Example_input.text
