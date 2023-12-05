open Core
open Util

let count_matches line =
  let nums = String.split line ~on:'|' |> List.map ~f:Parsing.ints_of_string in
  match nums with
  | [ _ :: winning_nums; my_nums ] ->
    let winning_nums = Hash_set.Poly.of_list winning_nums in
    List.count my_nums ~f:(Hash_set.mem winning_nums)
  | _ -> failwith ("Invalid line: " ^ line)
;;

let solve_1 input =
  String.split_lines input
  |> List.map ~f:(fun line -> 1 lsl (count_matches line - 1))
  |> Lists.sum
  |> Int.to_string
;;

let solve_2 input =
  let lines = String.split_lines input in
  let copies = Array.create ~len:(List.length lines) 1 in
  List.mapi lines ~f:(fun i line ->
    let matches = count_matches line in
    let curr_copies = copies.(i) in
    for j = i + 1 to i + matches do
      copies.(j) <- copies.(j) + curr_copies
    done;
    curr_copies)
  |> Lists.sum
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
