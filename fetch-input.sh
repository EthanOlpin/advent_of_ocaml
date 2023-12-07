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

base_dir="./bin/solutions"

echo -n "$(curl -s "https://adventofcode.com/$year/day/$day/input" -H "Cookie: session=$AOC_SESSION_ID")" >  "$base_dir/$year/$day/input.txt"