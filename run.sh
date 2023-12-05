#!/bin/bash

export TZ="America/New_York"

if [ -z "$1" ]; then
    year=$(date '+%Y')
else
    year=$1
fi

if [ -z "$2" ]; then
    day=$(date '+%-d')
else
    day=$2
fi

input_file="./bin/solutions/$year/$day/input.txt"
if [ ! -f "$input_file" ] || [ ! -s "$input_file" ]; then
    echo "Fetching input for $year day $day..."
    ./fetch-input.sh "$year" "$day"
fi

dune exec bin/solutions/$year/$day/main.exe --release
