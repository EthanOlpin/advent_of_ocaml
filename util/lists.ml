open Core

let max_exn l ~compare = Sequence.of_list l |> Sequences.max_exn ~compare
let min_exn l ~compare = Sequence.of_list l |> Sequences.min_exn ~compare

let min_max_exn l ~compare =
  Sequence.of_list l |> Sequences.min_max_exn ~compare
;;

let max_int_exn l = max_exn l ~compare:Int.compare
let min_int_exn l = min_exn l ~compare:Int.compare
let min_max_int_exn l = min_max_exn l ~compare:Int.compare

let fold_until_i l ~init ~f =
  Sequence.of_list l |> Sequences.fold_until_i ~init ~f
;;

let sum l = Sequence.of_list l |> Sequences.sum
let product l = Sequence.of_list l |> Sequences.product
let get_counts_map l m = Sequences.get_counts_map (Sequence.of_list l) m
let sort_desc l ~compare = List.sort l ~compare:(fun a b -> compare b a)
