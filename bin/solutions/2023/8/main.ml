open Core
open Util

module Network = struct
  type t = (string * string) Map.M(String).t

  let of_string s =
    String.split_lines s
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun map line ->
           Scanf.sscanf line "%3s = (%3s, %3s)" (fun parent left right ->
             Map.set map ~key:parent ~data:(left, right)))
  ;;

  let find t name = Map.find_exn t name

  let path_length t start ~directions ~stop =
    let rec aux node directions steps =
      if stop node
      then steps
      else (
        let direction, directions' =
          Sequence.next directions |> Option.value_exn
        in
        let left, right = find t node in
        let node' =
          match direction with
          | 'L' -> left
          | 'R' -> right
          | _ -> failwith "Invalid direction"
        in
        aux node' directions' (steps + 1))
    in
    aux start directions 0
  ;;
end

let parse_input input =
  match Parsing.split_double_newline input with
  | [ directions; nodes ] ->
    let directions = String.to_list directions |> Sequence.cycle_list_exn in
    let network = Network.of_string nodes in
    directions, network
  | _ -> failwith "Invalid input"
;;

let rec gcd u v = if v <> 0 then gcd v (u mod v) else u

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> m * n / gcd m n
;;

let lcm_of_list = function
  | [] -> 0
  | hd :: tl -> List.fold tl ~init:hd ~f:lcm
;;

let solve_1 input =
  let directions, network = parse_input input in
  let steps =
    Network.path_length network "AAA" ~directions ~stop:(String.equal "ZZZ")
  in
  Int.to_string steps
;;

let solve_2 input =
  let directions, network = parse_input input in
  let is_start s = String.nget s 2 |> Char.equal 'A' in
  let stop s = String.nget s 2 |> Char.equal 'Z' in
  Map.filter_keys network ~f:is_start
  |> Map.keys
  |> List.map ~f:(Network.path_length network ~directions ~stop)
  |> lcm_of_list
  |> Int.to_string
;;

Printing.output_solution
  ~part:1
  ~solve:solve_1
  ~input:Example_input.text
  ~example_input:Input.text
;;

Printing.output_solution
  ~part:2
  ~solve:solve_2
  ~input:Example_input.text
  ~example_input:Input.text
