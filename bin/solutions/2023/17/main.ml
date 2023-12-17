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
end

let find_shortest_path grid ~get_succesors =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let compare (d1, _) (d2, _) = Int.compare d1 d2 in
  let is_goal (state : State.t) = state.r = rows - 1 && state.c = cols - 1 in
  let heap = Pairing_heap.create ~cmp:compare () in
  let visited = Hash_set.create (module State) in
  let rec loop () =
    match Pairing_heap.pop heap with
    | None -> failwith "no path"
    | Some (dist, state) ->
      if is_goal state
      then dist
      else if Hash_set.mem visited state
      then loop ()
      else (
        Hash_set.add visited state;
        List.iter (get_succesors state dist) ~f:(Pairing_heap.add heap);
        loop ())
  in
  Pairing_heap.add heap (0, State.start);
  loop ()
;;

let solve_1 input =
  let grid = parse_digit_grid input in
  let get_succesors = State.get_successors ~max_steps:3 grid in
  find_shortest_path grid ~get_succesors |> Int.to_string
;;

let solve_2 input =
  let grid = parse_digit_grid input in
  let get_succesors = State.get_successors ~min_steps:4 ~max_steps:10 grid in
  find_shortest_path grid ~get_succesors |> Int.to_string
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
