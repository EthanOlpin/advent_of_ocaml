open Core

type 'a leftist_tree =
  | Leaf
  | Node of 'a leftist_tree * 'a * 'a leftist_tree * int

type 'a t =
  { cmp : 'a -> 'a -> int
  ; data : 'a leftist_tree
  }

let singleton cmp x = { cmp; data = Node (Leaf, x, Leaf, 1) }

let rank = function
  | Leaf -> 0
  | Node (_, _, _, r) -> r
;;

let rec merge_trees cmp t1 t2 =
  match t1, t2 with
  | Leaf, t | t, Leaf -> t
  | Node (l, k1, r, _), Node (_, k2, _, _) ->
    if cmp k1 k2 > 0
    then merge_trees cmp t2 t1
    else (
      let merged = merge_trees cmp r t2 in
      let rank_left = rank l
      and rank_right = rank merged in
      if rank_left >= rank_right
      then Node (l, k1, merged, rank_right + 1)
      else Node (merged, k1, l, rank_left + 1))
;;

let merge h1 h2 =
  let merged_trees = merge_trees h1.cmp h1.data h2.data in
  { cmp = h1.cmp; data = merged_trees }
;;

let insert t x = merge t (singleton t.cmp x)
let insert_list t xs = List.fold_left xs ~init:t ~f:insert

let peek = function
  | { cmp = _; data = Node (_, k, _, _) } -> Some k
  | { cmp = _; data = Leaf } -> None
;;

let peex_exn t = peek t |> Option.value_exn ~message:"empty heap"

let delete_min = function
  | { cmp; data = Node (l, _, r, _) } ->
    Some { cmp; data = merge_trees cmp l r }
  | { cmp = _; data = Leaf } -> None
;;

let delete_min_exn t = delete_min t |> Option.value_exn ~message:"empty heap"

let pop_exn t =
  let min = peex_exn t in
  let new_heap = delete_min_exn t in
  min, new_heap
;;
