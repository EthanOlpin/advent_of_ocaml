open Core

let in_bounds_2d arr r c =
  r >= 0 && r < Array.length arr && c >= 0 && c < Array.length arr.(r)
;;

let neighbor_positions_with_diags_2d arr r c =
  let neighbors =
    [ r - 1, c - 1
    ; r - 1, c
    ; r - 1, c + 1
    ; r, c - 1
    ; r, c + 1
    ; r + 1, c - 1
    ; r + 1, c
    ; r + 1, c + 1
    ]
  in
  List.filter neighbors ~f:(fun (r, c) -> in_bounds_2d arr r c)
;;

let neighbor_positions_2d arr r c =
  let neighbors = [ r - 1, c; r, c - 1; r, c + 1; r + 1, c ] in
  List.filter neighbors ~f:(fun (r, c) -> in_bounds_2d arr r c)
;;

let find_pos_2d arr ~f =
  Array.find_mapi arr ~f:(fun r ->
    Array.find_mapi ~f:(fun c x -> Option.some_if (f x) (r, c)))
;;

let find_pos_2d_exn arr ~f =
  Array.find_mapi_exn arr ~f:(fun r ->
    Array.find_mapi ~f:(fun c x -> Option.some_if (f x) (r, c)))
;;
