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
  let rec aux ranges acc =
    match ranges with
    | [] -> acc
    | start :: length :: ranges -> aux ranges ((start, start + length) :: acc)
    | _ -> failwith "Invalid seed ranges"
  in
  aux (Parsing.ints_of_string ranges) []
;;

let apply_map_to_range source_range sorted_map =
  let rec aux target_range sorted_map acc =
    match sorted_map with
    | [] -> target_range :: acc
    | (dest_start, source_start, length) :: rest_map ->
      let target_start, target_stop = target_range in
      let source_stop = source_start + length in
      let overlap_start = max source_start target_start in
      let overlap_stop = min source_stop target_stop in
      if overlap_start < overlap_stop
      then (
        let delta = dest_start - source_start in
        let mapped_range = overlap_start + delta, overlap_stop + delta in
        (* If we encounter an overlap we know any left-hanging segment
           cannot be applied to a range later in the map because the map
           is sorted. Map the overlapping segment to dest, if there's a
           hanging segment to the right of the overlap, keep looking to
           see if it overlaps with a range later in the map*)
        let acc' =
          if target_start < overlap_start
          then (target_start, overlap_start) :: mapped_range :: acc
          else mapped_range :: acc
        in
        if overlap_stop < target_stop
        then aux (overlap_stop, target_stop) rest_map acc'
        else acc')
      else aux target_range rest_map acc
  in
  aux source_range sorted_map []
;;

let apply_maps_to_ranges source_ranges sorted_maps =
  let rec aux sorted_maps source_ranges =
    match sorted_maps with
    | [] -> source_ranges
    | map :: rest_maps ->
      List.concat_map source_ranges ~f:(fun source_range ->
        apply_map_to_range source_range map)
      |> aux rest_maps
  in
  aux sorted_maps source_ranges
;;

let min_of_ranges ranges =
  List.fold ranges ~init:None ~f:(fun acc (start, _) ->
    match acc with
    | None -> Some start
    | Some acc -> Some (min acc start))
  |> Option.value_exn
;;

let range_compare (_, source_start, _) (_, source_start', _) =
  Int.compare source_start source_start'
;;

let solve_2 input =
  match Parsing.split_double_newline input with
  | [] -> failwith "Empty input"
  | seed_ranges :: maps ->
    let seed_ranges = parse_seed_ranges seed_ranges in
    let sorted_maps =
      List.map maps ~f:(fun map ->
        parse_map map |> List.sort ~compare:range_compare)
    in
    apply_maps_to_ranges seed_ranges sorted_maps
    |> min_of_ranges
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
