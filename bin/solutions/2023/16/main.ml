open Core
open Util

module Vec2 = struct
  include Tuple.Comparator (Int) (Int)

  type t = int * int [@@deriving compare, sexp]
end

module Vec2Set = Set.Make (Vec2)

module Beam = struct
  type t = Vec2.t * Vec2.t [@@deriving compare, sexp]
end

module BeamSet = struct
  include Set.Make (Beam)

  let positions t = Vec2Set.map t ~f:fst
end

let trace_beam start start_dir grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let out_of_bounds (r, c) = r < 0 || r >= rows || c < 0 || c >= cols in
  let rec aux (r, c) (dr, dc) visited =
    if out_of_bounds (r, c) || Set.mem visited ((r, c), (dr, dc))
    then visited
    else (
      let visited = Set.add visited ((r, c), (dr, dc)) in
      match grid.(r).(c), (dr, dc) with
      | '|', (0, 1) | '|', (0, -1) ->
        aux (r - 1, c) (-1, 0) (aux (r + 1, c) (1, 0) visited)
      | '-', (1, 0) | '-', (-1, 0) ->
        aux (r, c - 1) (0, -1) (aux (r, c + 1) (0, 1) visited)
      | '\\', (1, 0) | '/', (-1, 0) -> aux (r, c + 1) (0, 1) visited
      | '\\', (-1, 0) | '/', (1, 0) -> aux (r, c - 1) (0, -1) visited
      | '\\', (0, 1) | '/', (0, -1) -> aux (r + 1, c) (1, 0) visited
      | '\\', (0, -1) | '/', (0, 1) -> aux (r - 1, c) (-1, 0) visited
      | '.', _ | '|', _ | '-', _ -> aux (r + dr, c + dc) (dr, dc) visited
      | _ -> failwith "Unexpected character in input")
  in
  aux start start_dir BeamSet.empty |> BeamSet.positions |> Set.length
;;

let solve_1 input =
  let grid = String.split_lines input |> Array.of_list_map ~f:String.to_array in
  trace_beam (0, 0) (0, 1) grid |> Int.to_string
;;

(*Could be sped up quite a bit with a memo but that's a bit of a pain*)
let solve_2 input =
  let grid = Parsing.char_grid_of_string input in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let col_edges =
    List.range 0 rows
    |> List.concat_map ~f:(fun r -> [ r, 0; r, cols - 1 ])
    |> Vec2Set.of_list
  in
  let row_edges =
    List.range 0 cols
    |> List.concat_map ~f:(fun c -> [ 0, c; rows - 1, c ])
    |> Vec2Set.of_list
  in
  let edges = Vec2Set.union_list [ col_edges; row_edges ] in
  Set.fold edges ~init:Int.min_value ~f:(fun max_dist (r, c) ->
    let dirs =
      match r, c with
      (* Corners *)
      | 0, 0 -> [ 0, 1; 1, 0 ]
      | r, 0 when r = rows - 1 -> [ 0, 1; -1, 0 ]
      | 0, c when c = cols - 1 -> [ 0, -1; 1, 0 ]
      | r, c when r = rows - 1 && c = cols - 1 -> [ 0, -1; -1, 0 ]
      (* Edges *)
      | _, 0 -> [ 0, 1 ]
      | r, _ when r = rows - 1 -> [ 0, -1 ]
      | 0, _ -> [ 1, 0 ]
      | _, c when c = cols - 1 -> [ -1, 0 ]
      | _ -> failwith "Tried to get direction for non-edge"
    in
    List.fold dirs ~init:max_dist ~f:(fun max_dist dir ->
      Int.max max_dist (trace_beam (r, c) dir grid)))
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
