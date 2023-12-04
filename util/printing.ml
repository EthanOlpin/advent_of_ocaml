open Core

let plot_point_list ?(y_ascending = true) points =
  let min_max l =
    Sequence.of_list l |> Sequences.min_max_exn ~compare:Int.compare
  in
  let x1, x2 = List.map points ~f:fst |> min_max in
  let y1, y2 = List.map points ~f:snd |> min_max in
  let inner_width = x2 - x1 + 1 in
  let inner_height = y2 - y1 + 1 in
  let number_width n = Int.to_string n |> String.length in
  let side_margin = max (number_width y1) (number_width y2) in
  let column_width = max 2 (max (number_width x1) (number_width x2)) in
  let column_pad s = String.pad_left s ~len:column_width in
  let margin_pad s = String.pad_left s ~len:side_margin in
  let grid =
    Array.make_matrix
      ~dimx:(inner_height + 2)
      ~dimy:(inner_width + 2)
      (column_pad "")
  in
  grid.(0).(0) <- margin_pad "";
  grid.(inner_height + 1).(0) <- margin_pad "";
  for x = 0 to inner_width - 1 do
    let label = Int.to_string (x + x1) |> column_pad in
    grid.(0).(x + 1) <- label;
    grid.(inner_height + 1).(x + 1) <- label
  done;
  for y = 0 to inner_height - 1 do
    let label = Int.to_string (y + y1) in
    grid.(y + 1).(0) <- margin_pad label;
    grid.(y + 1).(inner_width + 1) <- column_pad label
  done;
  List.iter points ~f:(fun (x, y) ->
    let x = x - x1 + 1 in
    let y = y - y1 + 1 in
    grid.(y).(x) <- "#" |> column_pad);
  let rows = Array.map grid ~f:String.concat_array in
  (if y_ascending then Array.rev rows else rows)
  |> String.concat_array ~sep:"\n"
;;

let benchmarked ~f =
  let start = Time_float.now () in
  let result = f () in
  let stop = Time_float.now () in
  let duration = Time_float.diff stop start |> Time_float.Span.to_sec in
  result, duration
;;

let output_solution ~part ~solve ~input ~example_input =
  let example_result, example_duration =
    benchmarked ~f:(fun () -> solve example_input)
  in
  let result, duration = benchmarked ~f:(fun () -> solve input) in
  Stdio.printf
    "---Part %i---\n\
     Example (Finished in %.4f s):\n\
     %s\n\
     Result (Finished in %.4f s):\n\
     %s\n"
    part
    example_duration
    example_result
    duration
    result
;;
