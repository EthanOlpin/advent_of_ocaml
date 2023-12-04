open Core
open Util

let rec find_num_start row c =
  if c >= 0 && Char.is_digit row.(c) then find_num_start row (c - 1) else c + 1
;;

let num_starting_at row c =
  let rec loop acc c =
    if c < Array.length row && Char.is_digit row.(c)
    then loop ((acc * 10) + Char.get_digit_exn row.(c)) (c + 1)
    else acc
  in
  loop 0 c
;;

let get_symbol_positions arr =
  let symbol_position pos ch =
    match ch with
    | '0' .. '9' | '.' -> None
    | _ -> Some pos
  in
  Array.concat_mapi arr ~f:(fun r ->
    Array.filter_mapi ~f:(fun c -> symbol_position (r, c)))
  |> List.of_array
;;

let get_gear_positions arr =
  let is_gear (r, c) = Char.equal arr.(r).(c) '*' in
  get_symbol_positions arr |> List.filter ~f:is_gear
;;

let get_adjacent_number_positions arr (r, c) =
  Arrays.neighbor_positions_with_diags_2d arr r c
  |> List.filter_map ~f:(fun (r, c) ->
    Option.some_if (Char.is_digit arr.(r).(c)) (r, find_num_start arr.(r) c))
;;

let solve_1 input =
  let arr = Parsing.char_grid_of_string input in
  let symbol_positions = get_symbol_positions arr in
  let symbol_adjacent_number_locs =
    List.concat_map symbol_positions ~f:(get_adjacent_number_positions arr)
    |> Set.Poly.stable_dedup_list
  in
  let symbol_adjacent_numbers =
    List.map symbol_adjacent_number_locs ~f:(fun (r, c) ->
      num_starting_at arr.(r) c)
  in
  Lists.sum symbol_adjacent_numbers |> Int.to_string
;;

let solve_2 input =
  let arr = Parsing.char_grid_of_string input in
  let gear_positions = get_gear_positions arr in
  let gear_adjacent_number_locs =
    List.map gear_positions ~f:(fun pos ->
      get_adjacent_number_positions arr pos |> Set.Poly.stable_dedup_list)
  in
  let gear_ratios =
    List.filter_map gear_adjacent_number_locs ~f:(function
      | [ (r1, c1); (r2, c2) ] ->
        Some (num_starting_at arr.(r1) c1 * num_starting_at arr.(r2) c2)
      | _ -> None)
  in
  Lists.sum gear_ratios |> Int.to_string
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
