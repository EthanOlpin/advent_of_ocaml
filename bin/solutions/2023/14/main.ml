open Core
open Util

let compare_rocks a b =
  match a, b with
  | 'O', 'O' -> 0
  | 'O', _ -> -1
  | _, 'O' -> 1
  | _ -> 0
;;

let shift_rocks_left grid =
  let rec sort_segments row =
    let not_barrier c = Char.(c <> '#') in
    let segment = List.take_while row ~f:not_barrier in
    let rest = List.drop_while row ~f:not_barrier in
    let sorted_segment = List.sort segment ~compare:compare_rocks in
    match rest with
    | [] -> sorted_segment
    | '#' :: rest -> sorted_segment @ ('#' :: sort_segments rest)
    | _ -> failwith "Invalid input"
  in
  List.map grid ~f:sort_segments
;;

let rotate_left l = List.map l ~f:List.rev |> List.transpose_exn
let rotate_right l = List.transpose_exn l |> List.map ~f:List.rev

let perform_n_cycles n grid =
  let rec repeat_cycle i grid grid_index index_grid =
    let rec repeat_shift_rotate n grid =
      if n = 0
      then grid
      else shift_rocks_left grid |> rotate_right |> repeat_shift_rotate (n - 1)
    in
    let grid = repeat_shift_rotate 4 grid in
    match Map.find grid_index grid with
    | Some cycle_start ->
      let cycle_length = i - cycle_start in
      let grid_n_i = ((n - cycle_start - 1) % cycle_length) + cycle_start in
      Map.find_exn index_grid grid_n_i
    | None ->
      let grid_index = Map.set grid_index ~key:grid ~data:i in
      let index_grid = Map.set index_grid ~key:i ~data:grid in
      repeat_cycle (i + 1) grid grid_index index_grid
  in
  repeat_cycle 0 grid Map.Poly.empty Map.Poly.empty
;;

let compute_load grid =
  let num_cols = List.length (List.hd_exn grid) in
  List.foldi grid ~init:0 ~f:(fun _ acc row ->
    List.foldi row ~init:acc ~f:(fun c acc ch ->
      if Char.(ch = 'O') then acc + (num_cols - c) else acc))
;;

let solve_1 input =
  input
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> rotate_left
  |> shift_rocks_left
  |> compute_load
  |> Int.to_string
;;

let solve_2 input =
  input
  |> String.split_lines
  |> List.map ~f:String.to_list
  |> rotate_left
  |> perform_n_cycles 1000000000
  |> compute_load
  |> Int.to_string
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
