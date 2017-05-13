#!/bin/bash

set -e

function runExample {
    set +e
    ./output << DOC > $1
input
DOC
    exit_status=$?
    set -e
    if [ 0 != $exit_status ]; then
        echo "example returned non-zero exit code: $exit_status"
        exit $exit_status
    fi
}

for f in ./examples/*.el; do
    expectedOutput="$f.output"
    actualOutput=$(mktemp)
    if [ ! -f $expectedOutput ]; then
        echo "Missing output file $expectedOutput"
        exit 1
    fi
    echo "Compiling $f"
    ./elc $f
    echo "Running $f"
    runExample $actualOutput
    echo "Comparing output of $f"
    diff -u $expectedOutput $actualOutput
done

echo "Success"