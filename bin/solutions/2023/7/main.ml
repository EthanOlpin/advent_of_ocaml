open Core
open Util

let card_value card ~wildcard_joker =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> if wildcard_joker then 1 else 11
  | 'T' -> 10
  | c -> Char.get_digit_exn c
;;

let compare_card_value c1 c2 ~wildcard_joker =
  card_value c1 ~wildcard_joker - card_value c2 ~wildcard_joker
;;

type hand_type =
  | High_card
  | One_pair
  | Two_pair
  | Three_of_a_kind
  | Full_house
  | Four_of_a_kind
  | Five_of_a_kind
[@@deriving compare]

let promote_jokers card_counts =
  match Map.find card_counts 'J' with
  | None -> card_counts
  | Some joker_count ->
    let other_cards = Map.remove card_counts 'J' in
    let max_card, max_count =
      Map.fold
        other_cards
        ~init:('J', 0)
        ~f:(fun ~key ~data (max_card, max_count) ->
          if data > max_count then key, data else max_card, max_count)
    in
    let new_max_count = max_count + joker_count in
    Map.set other_cards ~key:max_card ~data:new_max_count
;;

let get_hand_type hand ~wildcard_joker =
  let card_counts = Lists.get_counts_map hand (module Char) in
  let card_counts =
    if wildcard_joker then promote_jokers card_counts else card_counts
  in
  let counts =
    Map.to_alist card_counts
    |> List.map ~f:snd
    |> List.sort ~compare:Int.compare
  in
  let hand_type =
    match counts with
    | [ 1; 1; 1; 1; 1 ] -> High_card
    | [ 1; 1; 1; 2 ] -> One_pair
    | [ 1; 2; 2 ] -> Two_pair
    | [ 1; 1; 3 ] -> Three_of_a_kind
    | [ 2; 3 ] -> Full_house
    | [ 1; 4 ] -> Four_of_a_kind
    | [ 5 ] -> Five_of_a_kind
    | _ -> failwith "Invalid hand"
  in
  hand_type
;;

let compare_hands h1 h2 ~wildcard_joker =
  let h1_type = get_hand_type h1 ~wildcard_joker in
  let h2_type = get_hand_type h2 ~wildcard_joker in
  let type_comparison = compare_hand_type h1_type h2_type in
  if type_comparison <> 0
  then type_comparison
  else
    List.zip_exn h1 h2
    |> List.find_map ~f:(fun (c1, c2) ->
      let comparison = compare_card_value c1 c2 ~wildcard_joker in
      Option.some_if (comparison <> 0) comparison)
    |> Option.value ~default:0
;;

let parse_hand_and_bid line =
  match String.split line ~on:' ' with
  | [ hand; bid ] -> String.to_list hand, Int.of_string bid
  | _ -> failwith "Invalid input"
;;

let get_winnings input ~wildcard_joker =
  String.split_lines input
  |> List.map ~f:parse_hand_and_bid
  |> List.sort ~compare:(fun (h1, _) (h2, _) ->
    compare_hands h1 h2 ~wildcard_joker)
  |> List.foldi ~init:0 ~f:(fun i acc (_, bid) ->
    let rank = i + 1 in
    acc + (rank * bid))
  |> Int.to_string
;;

let solve_1 input = get_winnings input ~wildcard_joker:false
let solve_2 input = get_winnings input ~wildcard_joker:true;;

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
