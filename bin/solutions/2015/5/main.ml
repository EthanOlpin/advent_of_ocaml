open Core
open Util

let is_vowel = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false
;;

let is_invalid_pair = function
  | 'a', 'b' | 'c', 'd' | 'p', 'q' | 'x', 'y' -> true
  | _ -> false
;;

let is_nice string =
  let rec aux has_pair vowel_count = function
    | [] -> has_pair && vowel_count >= 3
    | [ c ] -> has_pair && vowel_count + Bool.to_int (is_vowel c) >= 3
    | a :: b :: chars ->
      let has_pair' = has_pair || Char.(a = b) in
      let vowel_count' = vowel_count + Bool.to_int (is_vowel a) in
      (not (is_invalid_pair (a, b))) && aux has_pair' vowel_count' (b :: chars)
  in
  aux false 0 (String.to_list string)
;;

let is_nice_2 string =
  let rec aux has_symetric_triple has_repeated_pair pairs prev_pair = function
    | [] | [ _ ] -> has_symetric_triple && has_repeated_pair
    | [ a; b ] ->
      has_symetric_triple && (has_repeated_pair || Set.mem pairs (a, b))
    | a :: b :: c :: chars ->
      let has_symetric_triple' = has_symetric_triple || Char.(a = c) in
      let has_repeated_pair' = has_repeated_pair || Set.mem pairs (a, b) in
      let pairs' =
        Option.value_map prev_pair ~default:pairs ~f:(Set.add pairs)
      in
      (has_symetric_triple' && has_repeated_pair')
      || aux
           has_symetric_triple'
           has_repeated_pair'
           pairs'
           (Some (a, b))
           (b :: c :: chars)
  in
  let pairs = Set.empty (module Tuple.Comparator (Char) (Char)) in
  aux false false pairs None (String.to_list string)
;;

let solve_1 input =
  String.split_lines input |> List.count ~f:is_nice |> Int.to_string
;;

let solve_2 input =
  String.split_lines input |> List.count ~f:is_nice_2 |> Int.to_string
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
