# Advent of OCaml

[Advent of Code](https://adventofcode.com/) problems solved in OCaml using the [Jane Street Core alternative standard library](https://opensource.janestreet.com/core/)

Solutions may be inefficient, anti-idiomatic, hot-garbage, etc. refer to them at your own risk.

## Running the Solutions

1. Clone the repo
2. Switch to the new directory
3. Create a new switch 
    ```bash
    opam switch create advent_of_ocaml 5.1.0
    ```
4. Update your PATH
    ```bash
    eval $(opam env)
    ```
5. Install the project dependencies
    ```bash
    opam install . --locked
    ```
6. Make the runner script executable
    ```bash
    chmod +x ./run.sh
    ```
7. Run a solution
    ```bash
    ./run.sh <year> <day>
    ```