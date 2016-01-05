#!/bin/bash
#
# Filter stdin by requiring that one of the command line arguments must prefix
# each line.
#
# Usage: cat foo.txt | ./munge.sh 'bar' 'my_array\[[0-9]+\]\s*='

[[ $# > 0 ]] || { echo "no args given"; exit 1; }

pattern="^("

while [[ $# > 1 ]]
do
    p="$1"
    pattern+="$p|"
    shift
done

pattern+="$1).*"

exec grep --color=never -E "$pattern"
