open Core
open Util

let apply_map source map =
  List.find_map map ~f:(fun (dest_start, source_start, length) ->
    if source_start <= source && source < source_start + length
    then Some (dest_start + source - source_start)
    else None)
  |> Option.value ~default:source
;;

let parse_map map =
  match String.split_lines map with
  | [] -> failwith "Empty map"
  | _ :: ranges ->
    List.map ranges ~f:(fun range ->
      match Parsing.ints_of_string range with
      | [ dest_start; source_start; length ] -> dest_start, source_start, length
      | _ -> failwith "Invalid range")
;;

let solve_1 input =
  match Parsing.split_double_newline input with
  | [] -> failwith "Empty input"
  | seeds :: maps ->
    let seeds = Parsing.ints_of_string seeds in
    let maps = List.map maps ~f:parse_map in
    List.map seeds ~f:(fun seed -> List.fold maps ~init:seed ~f:apply_map)
    |> Lists.min_int_exn
    |> Int.to_string
;;

let parse_seed_ranges ranges =
  let rec aux ranges =
    match ranges with
    | [ start; length ] -> [ start, length ]
    | start :: length :: ranges -> (start, length) :: aux ranges
    | _ -> failwith "Invalid seed ranges"
  in
  aux (Parsing.ints_of_string ranges)
;;

let solve_2 input =
  match Parsing.split_double_newline input with
  | [] -> failwith "Empty input"
  | seed_ranges :: maps ->
    let seed_ranges = parse_seed_ranges seed_ranges in
    let maps = List.map maps ~f:parse_map in
    List.fold
      seed_ranges
      ~init:Int.max_value
      ~f:(fun curr_min (start, length) ->
        (* Lazy evaluation is the only thing standing between us and running out of memory *)
        Sequence.range start (start + length)
        |> Sequence.fold ~init:curr_min ~f:(fun curr_min seed ->
          let location = List.fold maps ~init:seed ~f:apply_map in
          min curr_min location))
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
