open Core
open Util

let pipes = Set.of_list (module Char) [ '|'; '-'; 'L'; 'J'; '7'; 'F' ]

let exit_positions (r, c) pipe =
  let dr1, dc1, dr2, dc2 =
    match pipe with
    | '|' -> 1, 0, -1, 0
    | '-' -> 0, 1, 0, -1
    | 'L' -> 0, 1, -1, 0
    | 'J' -> 0, -1, -1, 0
    | '7' -> 0, -1, 1, 0
    | 'F' -> 0, 1, 1, 0
    | _ -> failwith "Cannot connect to non-pipe"
  in
  [ r + dr1, c + dc1; r + dr2, c + dc2 ]
;;

let pos_equal (r1, c1) (r2, c2) = r1 = r2 && c1 = c2

let set_start_to_pipe (start_r, start_c) grid =
  let pipes_connect curr_pipe (curr_r, curr_c) (next_r, next_c) =
    let next_pipe = grid.(next_r).(next_c) in
    let curr_exits = exit_positions (curr_r, curr_c) curr_pipe in
    let next_exits = exit_positions (next_r, next_c) next_pipe in
    List.exists curr_exits ~f:(fun (r, c) ->
      List.exists next_exits ~f:(fun (r', c') ->
        pos_equal (r, c) (next_r, next_c) && pos_equal (r', c') (curr_r, curr_c)))
  in
  let neighbor_pipes =
    Arrays.neighbor_positions_2d grid start_r start_c
    |> List.filter ~f:(fun (r, c) -> Set.mem pipes grid.(r).(c))
  in
  let pipe =
    Set.find_exn pipes ~f:(fun p ->
      List.count neighbor_pipes ~f:(pipes_connect p (start_r, start_c)) = 2)
  in
  grid.(start_r).(start_c) <- pipe
;;

let get_points_on_path start_pos grid =
  let get_next_pos (enter_r, enter_c) (pipe_r, pipe_c) grid =
    let dr, dc =
      match grid.(pipe_r).(pipe_c) with
      | '|' -> if enter_r > pipe_r then -1, 0 else 1, 0
      | '-' -> if enter_c < pipe_c then 0, 1 else 0, -1
      | 'L' -> if enter_c > pipe_c then -1, 0 else 0, 1
      | 'J' -> if enter_c < pipe_c then -1, 0 else 0, -1
      | '7' -> if enter_c < pipe_c then 1, 0 else 0, -1
      | 'F' -> if enter_c > pipe_c then 1, 0 else 0, 1
      | _ -> failwith "Attempted to enter non-pipe"
    in
    pipe_r + dr, pipe_c + dc
  in
  let rec traverse prev_pos curr_pos visited =
    let next_pos = get_next_pos prev_pos curr_pos grid in
    if Set.mem visited next_pos
    then visited
    else traverse curr_pos next_pos (Set.add visited next_pos)
  in
  let start_r, start_c = start_pos in
  let first_pos =
    exit_positions start_pos grid.(start_r).(start_c) |> List.hd_exn
  in
  traverse start_pos first_pos (Set.Poly.of_list [ start_pos; first_pos ])
;;

let parse_grid input =
  let grid =
    String.split_lines input |> List.map ~f:String.to_array |> Array.of_list
  in
  let start_r, start_c = Arrays.find_pos_2d_exn grid ~f:(Char.equal 'S') in
  set_start_to_pipe (start_r, start_c) grid;
  (start_r, start_c), grid
;;

let solve_1 input =
  let (start_r, start_c), grid = parse_grid input in
  let num_points = get_points_on_path (start_r, start_c) grid |> Set.length in
  num_points / 2 |> Int.to_string
;;

let count_spaces_in_loop points_on_path grid =
  let on_path = Set.mem points_on_path in
  let cols = Array.length grid.(0) in
  let consume_segment (r, c) in_loop =
    let start_pipe = grid.(r).(c) in
    match start_pipe with
    | '|' -> (r, c + 1), not in_loop
    | 'L' | 'F' ->
      let crossed_boundary start_pipe end_pipe =
        match start_pipe, end_pipe with
        | 'L', '7' | 'F', 'J' -> not in_loop
        | _ -> in_loop
      in
      let rec aux (r, c) =
        match grid.(r).(c) with
        | '-' -> aux (r, c + 1)
        | ('7' | 'J') as end_pipe ->
          (r, c + 1), crossed_boundary start_pipe end_pipe
        | _ -> failwith "Invalid pipe"
      in
      aux (r, c + 1)
    | _ -> failwith "Invalid pipe"
  in
  let rec scan_row (r, c) in_loop acc =
    if c > cols
    then acc
    else if on_path (r, c)
    then (
      let (r', c'), in_loop' = consume_segment (r, c) in_loop in
      scan_row (r', c') in_loop' acc)
    else scan_row (r, c + 1) in_loop (acc + Bool.to_int in_loop)
  in
  Array.foldi grid ~init:0 ~f:(fun r acc _ -> scan_row (r, 0) false acc)
;;

let solve_2 input =
  let (start_r, start_c), grid = parse_grid input in
  let points_on_path = get_points_on_path (start_r, start_c) grid in
  count_spaces_in_loop points_on_path grid |> Int.to_string
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
