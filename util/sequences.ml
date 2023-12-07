open Core

let max_exn seq ~compare =
  Sequence.max_elt seq ~compare
  |> Option.value_exn ~message:"max_exn: empty sequence"
;;

let min_exn seq ~compare =
  Sequence.min_elt seq ~compare
  |> Option.value_exn ~message:"min_exn: empty sequence"
;;

let min_max seq ~compare =
  let min a b = if compare a b < 0 then a else b in
  let max a b = if compare a b > 0 then a else b in
  Sequence.fold
    ~f:(fun min_max x ->
      match min_max with
      | None -> Some (x, x)
      | Some (curr_min, curr_max) -> Some (min curr_min x, max curr_max x))
    ~init:None
    seq
;;

let min_max_exn seq ~compare =
  min_max seq ~compare
  |> Option.value_exn ~message:"min_max_exn: empty sequence"
;;

let max_int_exn seq = max_exn seq ~compare:Int.compare
let min_int_exn seq = min_exn seq ~compare:Int.compare
let min_max_int_exn seq = min_max_exn seq ~compare:Int.compare

let fold_until_i seq ~init ~f =
  Sequence.fold_until
    seq
    ~init:(0, init)
    ~f:(fun (i, acc) x ->
      match f i acc x with
      | `Continue acc -> Continue (i + 1, acc)
      | `Stop acc -> Stop acc)
    ~finish:(fun _ -> failwith "fold_until_i: did not stop")
;;

let sum seq = Sequence.fold seq ~init:0 ~f:( + )
let product seq = Sequence.fold seq ~init:1 ~f:( * )

let get_counts_map seq m =
  Sequence.fold seq ~init:(Map.empty m) ~f:(fun counts_map x ->
    Map.update counts_map x ~f:(function
      | None -> 1
      | Some count -> count + 1))
;;
