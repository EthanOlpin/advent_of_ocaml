open Core
open Util

let get_new_pos (x, y) = function
  | '^' -> x, y + 1
  | 'v' -> x, y - 1
  | '>' -> x + 1, y
  | '<' -> x - 1, y
  | _ -> failwith "invalid input"
;;

let get_visited_positions dirs =
  List.fold_map ~init:(0, 0) dirs ~f:(fun pos c ->
    let new_pos = get_new_pos pos c in
    new_pos, new_pos)
  |> snd
;;

let solve_1 input =
  String.to_list input
  |> get_visited_positions
  |> Set.Poly.stable_dedup_list
  |> List.length
  |> Int.to_string
;;

let solve_2 input =
  let _, santa_dirs, robot_dirs =
    String.to_list input
    |> List.fold_right
         ~init:(true, [], [])
         ~f:(fun c (santa_turn, santa_input, robot_input) ->
           if santa_turn
           then false, c :: santa_input, robot_input
           else true, santa_input, c :: robot_input)
  in
  get_visited_positions santa_dirs @ get_visited_positions robot_dirs
  |> Set.Poly.stable_dedup_list
  |> List.length
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
