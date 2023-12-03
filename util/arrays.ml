open Core

let neighbor_positions_with_diags_2d arr i j =
  let neighbors =
    [ i - 1, j - 1
    ; i - 1, j
    ; i - 1, j + 1
    ; i, j - 1
    ; i, j + 1
    ; i + 1, j - 1
    ; i + 1, j
    ; i + 1, j + 1
    ]
  in
  List.filter neighbors ~f:(fun (i, j) ->
    i >= 0 && i < Array.length arr && j >= 0 && j < Array.length arr.(0))
;;
