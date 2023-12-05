open Core
open Util

let solve_1 input =
  input
  |> Parsing.chars_of_string
  |> List.fold ~init:0 ~f:(fun acc ->
      function
      | '(' -> acc + 1
      | ')' -> acc - 1
      | _ -> acc)
  |> Int.to_string
;;

let solve_2 input =
  input
  |> Parsing.chars_of_string
  |> Lists.fold_until_i ~init:0 ~f:(fun i acc c ->
    match c with
    | '(' -> `Continue (acc + 1)
    | ')' -> if acc - 1 = -1 then `Stop (i + 1) else `Continue (acc - 1)
    | _ -> failwith "invalid input")
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
