open Core
open Util

let solve_1 input =
  String.split_lines input
  |> List.fold ~init:0 ~f:(fun sum line ->
    let digits =
      Parsing.digits_of_string line
      |> Debugging.with_printed_list ~to_string:Int.to_string
    in
    let num = (10 * List.hd_exn digits) + List.last_exn digits in
    sum + num)
  |> Int.to_string
;;

let digit_of_chars = function
  | 'o' :: 'n' :: 'e' :: _ | '1' :: _ -> Some 1
  | 't' :: 'w' :: 'o' :: _ | '2' :: _ -> Some 2
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ | '3' :: _ -> Some 3
  | 'f' :: 'o' :: 'u' :: 'r' :: _ | '4' :: _ -> Some 4
  | 'f' :: 'i' :: 'v' :: 'e' :: _ | '5' :: _ -> Some 5
  | 's' :: 'i' :: 'x' :: _ | '6' :: _ -> Some 6
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ | '7' :: _ -> Some 7
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ | '8' :: _ -> Some 8
  | 'n' :: 'i' :: 'n' :: 'e' :: _ | '9' :: _ -> Some 9
  | _ -> None
;;

let filter_scan l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | _ :: tl as l ->
      (match f l with
       | Some x -> aux (x :: acc) tl
       | None -> aux acc tl)
  in
  aux [] l
;;

let solve_2 input =
  String.split_lines input
  |> List.fold ~init:0 ~f:(fun sum line ->
    let chars = String.to_list line in
    let digits = filter_scan chars ~f:digit_of_chars in
    let num = (10 * List.hd_exn digits) + List.last_exn digits in
    sum + num)
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
