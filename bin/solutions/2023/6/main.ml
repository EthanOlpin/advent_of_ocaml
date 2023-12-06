open Core
open Util

let ways_to_beat_record time record_speed =
  let rec aux time_remaining launch_speed acc =
    if time_remaining = 0
    then acc
    else if launch_speed * time_remaining > record_speed
    then aux (time_remaining - 1) (launch_speed + 1) (acc + 1)
    else aux (time_remaining - 1) (launch_speed + 1) acc
  in
  aux 0 time 0
;;

let solve_1 input =
  match String.split_lines input |> List.map ~f:Parsing.ints_of_string with
  | [ times; distances ] ->
    List.map2_exn times distances ~f:ways_to_beat_record
    |> Lists.product
    |> Int.to_string
  | _ -> failwith "Invalid input"
;;

let solve_2 input =
  match
    String.split_lines input |> List.map ~f:Parsing.int_of_sparse_string
  with
  | [ time; distance ] -> ways_to_beat_record time distance |> Int.to_string
  | _ -> failwith "Invalid input"
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
