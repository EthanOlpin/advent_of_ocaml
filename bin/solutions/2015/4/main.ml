open Core
open Util

let find_key_suffix prefix hash_prefix =
  let rec aux suffix =
    let hash = Md5.(digest_string (prefix ^ Int.to_string suffix) |> to_hex) in
    if String.is_prefix hash ~prefix:hash_prefix
    then suffix
    else aux (suffix + 1)
  in
  aux 0
;;

let solve_1 input = find_key_suffix input "00000" |> Int.to_string
let solve_2 input = find_key_suffix input "000000" |> Int.to_string;;

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
