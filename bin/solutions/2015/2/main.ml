open Core
open Util

let parse_dimensions line =
  match Parsing.ints_of_string line with
  | [ l; w; h ] -> l, w, h
  | _ -> failwith ("Invalid line: " ^ line)
;;

let solve_1 input =
  String.split_lines input
  |> List.fold ~init:0 ~f:(fun acc line ->
    let l, w, h = parse_dimensions line in
    let side_areas = [ l * w; w * h; h * l ] in
    let total_area = 2 * Lists.sum side_areas in
    acc + total_area + Lists.min_int_exn side_areas)
  |> Int.to_string
;;

let min2 l =
  let rec aux l min1 min2 =
    match l with
    | [] -> min1, min2
    | x :: xs ->
      if x < min1
      then aux xs x min1
      else if x < min2
      then aux xs min1 x
      else aux xs min1 min2
  in
  match l with
  | a :: b :: xs -> aux xs (min a b) (max a b)
  | _ -> failwith "Not enough elements for min2"
;;

let solve_2 input =
  String.split_lines input
  |> List.fold ~init:0 ~f:(fun acc line ->
    let l, w, h = parse_dimensions line in
    let min1, min2 = min2 [ l; w; h ] in
    let total_ribbon = 2 * (min1 + min2) in
    let bow = l * w * h in
    acc + total_ribbon + bow)
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
