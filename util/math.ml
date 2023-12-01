open Core

let sum = Sequence.fold ~init:0 ~f:Int.( + )
let product = Sequence.fold ~init:1 ~f:Int.( * )
