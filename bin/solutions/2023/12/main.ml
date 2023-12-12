open Core
open Util

let count_arrangements springs groups =
  let memo = Hashtbl.Poly.create () in
  let rec aux springs groups curr_group =
    let calculate () =
      match springs, groups with
      | [], [] -> 1
      | [], [ group ] when curr_group = group -> 1
      | [], _ | '#' :: _, [] -> 0
      | '.' :: springs, groups when curr_group = 0 -> aux springs groups 0
      | '.' :: springs, group :: groups_tl ->
        if curr_group = group then aux springs groups_tl 0 else 0
      | '#' :: springs, group :: _ ->
        if curr_group < group then aux springs groups (curr_group + 1) else 0
      | '?' :: springs, groups ->
        aux ('#' :: springs) groups curr_group
        + aux ('.' :: springs) groups curr_group
      | _ -> failwith "Invalid input"
    in
    let key = springs, groups, curr_group in
    Hashtbl.find_or_add memo key ~default:calculate
  in
  aux springs groups 0
;;

let rec clone_join ~n ~sep s =
  if n = 1 then s else s ^ sep ^ clone_join ~n:(n - 1) ~sep s
;;

let parse_row row ~unfold_factor =
  match String.split row ~on:' ' with
  | [ springs; groups ] ->
    let springs = clone_join ~n:unfold_factor ~sep:"?" springs in
    let groups = clone_join ~n:unfold_factor ~sep:"," groups in
    String.to_list springs, Parsing.ints_of_string groups
  | _ -> failwith "Invalid row"
;;

let parse_rows input ~unfold_factor =
  String.split_lines input |> List.map ~f:(parse_row ~unfold_factor)
;;

let solve input ~unfold_factor =
  parse_rows input ~unfold_factor
  |> List.sum
       (module Int)
       ~f:(fun (springs, groups) -> count_arrangements springs groups)
  |> Int.to_string
;;

let solve_1 input = solve input ~unfold_factor:1
let solve_2 input = solve input ~unfold_factor:5;;

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
