(lang dune 3.11)

(name advent_of_ocaml)

(generate_opam_files true)

(source
 (github EthanOlpin/advent_of_ocaml))

(authors "EthanOlpin")

(maintainers "Ethan Olpin")

(license MIT)

(package
 (name advent_of_ocaml)
 (allow_empty)
 (description "Advent of Code problems solved in OCaml")
 (depends ocaml dune core re ppx_deriving)
 (tags
  (ocaml "advent of code")))
