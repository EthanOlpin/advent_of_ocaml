#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 year day"
    exit 1
fi

year=$1
day=$2
base_dir="./bin/solutions"

mkdir -p "$base_dir/$year/$day"

cp ./template/dune.txt "$base_dir/$year/$day/dune"
cp ./template/main.txt "$base_dir/$year/$day/main.ml"

touch "$base_dir/$year/$day/example_input.txt"
touch "$base_dir/$year/$day/input.txt"

./fetch-input.sh "$year" "$day"

dune build --release
