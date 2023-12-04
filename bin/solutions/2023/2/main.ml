open Core
open Util

let color_maxes line =
  let re = Re.Perl.compile_pat "\\d+ (blue|red|green)" in
  let color_maxes =
    Re.matches re line
    |> List.map ~f:(fun m -> Scanf.sscanf m "%i %s" Tuples.make_2_rev)
    |> Hashtbl.Poly.of_alist_multi
    |> Hashtbl.map ~f:Lists.max_int_exn
  in
  ( Hashtbl.find_exn color_maxes "red"
  , Hashtbl.find_exn color_maxes "green"
  , Hashtbl.find_exn color_maxes "blue" )
;;

let solve_1 input =
  String.split_lines input
  |> List.foldi ~init:0 ~f:(fun i acc line ->
    let red_max, green_max, blue_max = color_maxes line in
    if red_max > 12 || green_max > 13 || blue_max > 14 then acc else 1 + i + acc)
  |> Int.to_string
;;

let solve_2 input =
  String.split_lines input
  |> List.fold ~init:0 ~f:(fun acc line ->
    let red_max, green_max, blue_maxes = color_maxes line in
    (red_max * green_max * blue_maxes) + acc)
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
