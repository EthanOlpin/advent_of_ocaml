open Core
open Util

let differences l =
  let rec aux acc = function
    | [] | [ _ ] -> List.rev acc
    | x :: y :: tl -> aux ((x - y) :: acc) (y :: tl)
  in
  aux [] l
;;

let predict_left_value history =
  let rec aux acc = function
    | [] -> failwith "Not enough data"
    | x :: tl when List.for_all tl ~f:(Int.equal x) -> x + acc
    | x :: tl as l -> aux (x + acc) (differences l)
  in
  aux 0 history
;;

let parse_histories input =
  String.split_lines input |> List.map ~f:Parsing.ints_of_string
;;

let solve_1 input =
  parse_histories input
  |> List.fold ~init:0 ~f:(fun acc history ->
    acc + predict_left_value (List.rev history))
  |> Int.to_string
;;

let solve_2 input =
  parse_histories input
  |> List.fold ~init:0 ~f:(fun acc history -> acc + predict_left_value history)
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
