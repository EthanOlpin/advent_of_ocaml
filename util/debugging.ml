open Core

let with_printed_int i =
  Stdio.print_endline (Int.to_string i);
  i
;;

let with_printed_string s =
  Stdio.print_endline s;
  s
;;

let with_printed_char c =
  Stdio.print_endline (Char.to_string c);
  c
;;

let with_printed_list l to_string =
  List.map l ~f:to_string |> String.concat ~sep:"; " |> Stdio.printf "[%s]\n";
  l
;;
