#!/bin/bash
# tested against:

if [ $# -ne 3 ]; then
    echo "Usage: run.sh <home> <out> <fSize>"
    echo "    <home>: top-level of codebase" 
    echo "    <out>: where to store all results" 
    echo "    <fsize>: frontier size"
    echo "EXAMPLE: run.sh ~/path/to/strappy/ src/experiments/Number/test/ 1000"
else
    # Make data directories for each frontier size
    fullOut="$1$2"
    mkdir $fullOut

    # CD to src/ directory
    cd "$1/src/"

    # Compile number model
    rm $1/src/Strappy/Number
    make clean
    make number

    # Run script
    ./Strappy/Number 0 1.5 1.0 $3 $fullOut | tee $fullOut/log
fi
