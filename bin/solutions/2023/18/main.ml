open Core
open Util

let parse_instruction line ~hex_steps =
  let re = Re.Perl.compile_pat "([RLDU]) (\\d+) \\(#(.*)\\)" in
  let m = Re.exec re line in
  match Re.Group.all m with
  | [| _; dir; steps; color |] ->
    if hex_steps
    then (
      let steps, dir = Scanf.sscanf color "%5x%c" Tuples.make_2 in
      let dir =
        match dir with
        | '0' -> 'R'
        | '1' -> 'D'
        | '2' -> 'L'
        | '3' -> 'U'
        | _ -> failwith "Invalid direction"
      in
      dir, steps)
    else Char.of_string dir, Int.of_string steps
  | _ -> failwith "Invalid instruction"
;;

let get_visited_points instructions =
  let rec aux perimiter visited (r, c) instructions =
    match instructions with
    | [] -> perimiter, visited
    | instruction :: instructions ->
      let nr, nc =
        match instruction with
        | 'R', steps -> r, c + steps
        | 'L', steps -> r, c - steps
        | 'U', steps -> r - steps, c
        | 'D', steps -> r + steps, c
        | _ -> failwith "Invalid instruction"
      in
      let visited = (nr, nc) :: visited in
      let perimiter = perimiter + abs (nr - r) + abs (nc - c) in
      aux perimiter visited (nr, nc) instructions
  in
  aux 0 [] (0, 0) instructions
;;

let rotate_left l = List.tl_exn l @ [ List.hd_exn l ]
let rotate_right l = List.last_exn l :: List.take l (List.length l - 1)

let area (perimiter, points) =
  let rows = List.map points ~f:fst in
  let cols = List.map points ~f:snd in
  let left_cols = rotate_left cols in
  let right_cols = rotate_right cols in
  (*https://en.wikipedia.org/wiki/Shoelace_formula*)
  let total_area =
    (List.map3_exn rows left_cols right_cols ~f:(fun r lc rc -> r * (lc - rc))
     |> Lists.sum
     |> abs)
    / 2
  in
  (*https://en.wikipedia.org/wiki/Pick%27s_theorem*)
  let interior_points = total_area - (perimiter / 2) + 1 in
  interior_points + perimiter
;;

let solve input ~hex_steps =
  String.split_lines input
  |> List.map ~f:(parse_instruction ~hex_steps)
  |> get_visited_points
  |> area
  |> Int.to_string
;;

let solve_1 input = solve input ~hex_steps:false
let solve_2 input = solve input ~hex_steps:true;;

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
