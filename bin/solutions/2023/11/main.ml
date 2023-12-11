open Core
open Util

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let get_offset_galaxy_positions grid offset_multiplier =
  let is_empty = List.for_all ~f:(Char.equal '.') in
  let rec get_offsets offset acc = function
    | [] -> List.rev acc
    | row :: rest ->
      let offset' = offset + if is_empty row then offset_multiplier else 1 in
      get_offsets offset' (offset :: acc) rest
  in
  let row_offsets = get_offsets 0 [] grid in
  let col_offsets = get_offsets 0 [] (List.transpose_exn grid) in
  List.zip_exn row_offsets grid
  |> List.concat_map ~f:(fun (r, row) ->
    List.zip_exn col_offsets row
    |> List.filter_map ~f:(fun (c, ch) -> Option.some_if Char.(ch = '#') (r, c)))
;;

let get_disctint_pairs positions =
  let distinct_ordered_pair p1 p2 =
    let (r1, c1), (r2, c2) = p1, p2 in
    if r1 < r2 || (r1 = r2 && c1 < c2)
    then Some (p1, p2)
    else if r1 > r2 || c1 > c2
    then Some (p2, p1)
    else None
  in
  List.concat_map positions ~f:(fun p1 ->
    List.filter_map positions ~f:(distinct_ordered_pair p1))
  |> Set.Poly.stable_dedup_list
;;

let solve input offset_multiplier =
  let grid = input |> String.split_lines |> List.map ~f:String.to_list in
  get_offset_galaxy_positions grid offset_multiplier
  |> get_disctint_pairs
  |> List.fold ~init:0 ~f:(fun acc (p1, p2) -> acc + manhattan_distance p1 p2)
  |> Int.to_string
;;

let solve_1 input = solve input 2
let solve_2 input = solve input 1_000_000;;

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
