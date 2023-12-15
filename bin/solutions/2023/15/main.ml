open Core
open Util

let hash s =
  String.fold s ~init:0 ~f:(fun acc c -> (Char.to_int c + acc) * 17 % 256)
;;

type operation =
  | Remove
  | Update of int

let parse_label_operation s =
  let re = Re.Perl.compile_pat "([a-z]+)([=-])(\\d+)?" in
  let groups = Re.exec re s in
  match Re.Group.all groups with
  | [| _; label; "-"; _ |] -> label, Remove
  | [| _; label; "="; value |] -> label, Update (Int.of_string value)
  | _ -> failwith "invalid op"
;;

let box_remove box label = List.Assoc.remove box label ~equal:String.equal

let box_update l label value =
  let is_not_target (l, _) = not String.(l = label) in
  let front = List.take_while l ~f:is_not_target in
  let back = List.drop_while l ~f:is_not_target in
  match back with
  | [] -> (label, value) :: front
  | _ :: rest -> front @ ((label, value) :: rest)
;;

let perform_operations boxes label_operations =
  let rec aux = function
    | [] -> ()
    | (label, op) :: rest ->
      let hashed_label = hash label in
      let box = boxes.(hashed_label) in
      boxes.(hashed_label)
      <- (match op with
          | Remove -> box_remove box label
          | Update value -> box_update box label value);
      aux rest
  in
  aux label_operations
;;

let solve_1 input =
  String.split input ~on:',' |> List.map ~f:hash |> Lists.sum |> Int.to_string
;;

let sum_focal_powers boxes =
  Array.foldi ~init:0 boxes ~f:(fun i acc box ->
    List.rev box
    |> List.foldi ~init:acc ~f:(fun j acc (_, value) ->
      acc + ((i + 1) * (j + 1) * value)))
;;

let solve_2 input =
  let label_operations =
    String.split input ~on:',' |> List.map ~f:parse_label_operation
  in
  let boxes = Array.create ~len:256 [] in
  perform_operations boxes label_operations;
  sum_focal_powers boxes |> Int.to_string
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
