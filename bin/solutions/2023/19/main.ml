open Core
open Util

type part =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

type dest =
  | Accepted
  | Rejected
  | Workflow of string

type op =
  | Gt of int
  | Lt of int

type rule =
  | Move of char * op * dest
  | Default of dest

let apply_op x = function
  | Gt comparator -> x > comparator
  | Lt comparator -> x < comparator
;;

let rec apply_workflows workflows workflow part =
  let map_to_dest part = function
    | Accepted -> true
    | Rejected -> false
    | Workflow workflow -> apply_workflows workflows workflow part
  in
  let rec apply_rules rules =
    match rules with
    | [] -> failwith "No matching rule"
    | Default dest :: _ -> map_to_dest part dest
    | Move (category, op, dest) :: rules ->
      let category_rank =
        match category with
        | 'x' -> part.x
        | 'm' -> part.m
        | 'a' -> part.a
        | 's' -> part.s
        | _ -> failwith "Invalid category"
      in
      if apply_op category_rank op
      then map_to_dest part dest
      else apply_rules rules
  in
  apply_rules (Map.find_exn workflows workflow)
;;

let parse_rules rules =
  let parse_dest = function
    | "A" -> Accepted
    | "R" -> Rejected
    | dest -> Workflow dest
  in
  let parse_rule rule =
    let re = Re.Perl.compile_pat "([xasm])([<>])(\\d+):(\\w+)" in
    let matches = Re.exec_opt re rule in
    match matches with
    | None -> Default (parse_dest rule)
    | Some matches ->
      (match Re.Group.all matches with
       | [| _; category; op; comparator; dest |] ->
         let category = Char.of_string category in
         let comparator = Int.of_string comparator in
         let op = if String.(op = ">") then Gt comparator else Lt comparator in
         let dest = parse_dest dest in
         Move (category, op, dest)
       | _ -> failwith "Invalid match")
  in
  String.split rules ~on:',' |> List.map ~f:parse_rule
;;

let parse_workflows lines =
  let re = Re.Perl.compile_pat "(\\w+)\\{(.*)\\}" in
  let parse_workflow line =
    let matches = Re.exec re line in
    match Re.Group.all matches with
    | [| _; workflow; rules |] -> workflow, parse_rules rules
    | _ -> failwith "Invalid workflow"
  in
  List.map lines ~f:parse_workflow |> Map.of_alist_exn (module String)
;;

let parse_parts lines =
  let parse_part line =
    match Parsing.ints_of_string line with
    | [ x; m; a; s ] -> { x; m; a; s }
    | _ -> failwith "Invalid match"
  in
  List.map lines ~f:parse_part
;;

let parse_input input =
  match Parsing.split_double_newline input with
  | [ workflows; parts ] ->
    let workflows = parse_workflows (String.split_lines workflows) in
    let parts = parse_parts (String.split_lines parts) in
    workflows, parts
  | _ -> failwith "Invalid input"
;;

type part_ranges =
  { x : int * int
  ; m : int * int
  ; a : int * int
  ; s : int * int
  }

let count_combinations { x; m; a; s } =
  let range_len (lo, hi) = hi - lo + 1 in
  range_len x * range_len m * range_len a * range_len s
;;

let apply_op_to_range (lo, hi) = function
  | Gt comparator -> (max lo (comparator + 1), hi), (min hi comparator, hi)
  | Lt comparator -> (lo, min hi (comparator - 1)), (max lo comparator, hi)
;;

let invalid_part_ranges part_ranges =
  List.exists
    ~f:(fun (lo, hi) -> lo > hi)
    [ part_ranges.x; part_ranges.m; part_ranges.a; part_ranges.s ]
;;

let rec apply_workflows_to_ranges part_ranges workflow workflows =
  let map_to_dest part_ranges = function
    | Accepted -> count_combinations part_ranges
    | Rejected -> 0
    | Workflow workflow ->
      apply_workflows_to_ranges part_ranges workflow workflows
  in
  let rec apply_rules part_ranges rules =
    if invalid_part_ranges part_ranges
    then 0
    else (
      match rules with
      | [] -> failwith "No matching rule"
      | Default dest :: _ -> map_to_dest part_ranges dest
      | Move (category, op, dest) :: rules ->
        let inner, outer =
          match category with
          | 'x' ->
            let x_inner, x_outer = apply_op_to_range part_ranges.x op in
            { part_ranges with x = x_inner }, { part_ranges with x = x_outer }
          | 'm' ->
            let m_inner, m_outer = apply_op_to_range part_ranges.m op in
            { part_ranges with m = m_inner }, { part_ranges with m = m_outer }
          | 'a' ->
            let a_inner, a_outer = apply_op_to_range part_ranges.a op in
            { part_ranges with a = a_inner }, { part_ranges with a = a_outer }
          | 's' ->
            let s_inner, s_outer = apply_op_to_range part_ranges.s op in
            { part_ranges with s = s_inner }, { part_ranges with s = s_outer }
          | _ -> failwith "Invalid category"
        in
        map_to_dest inner dest + apply_rules outer rules)
  in
  apply_rules part_ranges (Map.find_exn workflows workflow)
;;

let solve_1 input =
  let workflows, parts = parse_input input in
  List.filter parts ~f:(apply_workflows workflows "in")
  |> List.fold ~init:0 ~f:(fun acc part ->
    acc + (part.x + part.m + part.a + part.s))
  |> Int.to_string
;;

let solve_2 input =
  let workflows, _ = parse_input input in
  let part_ranges = { x = 1, 4000; m = 1, 4000; a = 1, 4000; s = 1, 4000 } in
  apply_workflows_to_ranges part_ranges "in" workflows |> Int.to_string
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
