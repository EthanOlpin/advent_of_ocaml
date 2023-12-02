open Core
open Util

let solve_1 () =
  Input.text
  |> Parsing.chars_of_string
  |> List.fold ~init:0 ~f:(fun acc ->
      function
      | '(' -> acc + 1
      | ')' -> acc - 1
      | _ -> acc)
  |> Int.to_string
;;

let solve_2 () =
  Input.text
  |> Parsing.chars_of_string
  |> Lists.fold_until_i ~init:0 ~f:(fun i acc c ->
    match c with
    | '(' -> `Continue (acc + 1)
    | ')' -> if acc - 1 = -1 then `Stop (i + 1) else `Continue (acc - 1)
    | _ -> failwith "invalid input")
  |> Int.to_string
;;

Stdio.print_endline ("---Part 1---\n" ^ Printing.benchmarked ~f:solve_1);;
Stdio.print_endline ("---Part 2---\n" ^ Printing.benchmarked ~f:solve_2)
