open Core
open Util

let char_list_diff s1 s2 =
  List.fold2_exn s1 s2 ~init:0 ~f:(fun acc c1 c2 ->
    if Char.equal c1 c2 then acc else acc + 1)
;;

let both_non_empty l1 l2 = not (List.is_empty l1 || List.is_empty l2)

let is_reflection left right ~target_error =
  let rec aux total_error = function
    | row_l :: left, row_r :: right ->
      let total_error = total_error + char_list_diff row_l row_r in
      total_error <= target_error && aux total_error (left, right)
    | _ -> target_error = total_error
  in
  both_non_empty left right && aux 0 (left, right)
;;

let get_reflection rows ~target_error =
  let rec aux i tail rows =
    match rows with
    | [] -> 0
    | row :: rows_tl ->
      if is_reflection rows tail ~target_error
      then i
      else aux (i + 1) (row :: tail) rows_tl
  in
  aux 0 [] rows
;;

let solve input ~target_error =
  let groups = Parsing.split_double_newline input in
  List.fold groups ~init:0 ~f:(fun acc group ->
    let rows = String.split_lines group |> List.map ~f:String.to_list in
    match get_reflection rows ~target_error with
    | 0 ->
      let cols = List.transpose_exn rows in
      let col_reflection = get_reflection cols ~target_error in
      acc + col_reflection
    | row_reflection -> acc + (row_reflection * 100))
  |> Int.to_string
;;

let solve_1 input = solve input ~target_error:0
let solve_2 input = solve input ~target_error:1;;

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
