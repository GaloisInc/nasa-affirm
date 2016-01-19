#!/bin/bash
#
# Reads a SAL 1-step counter example from stdin and uses `vimdiff` to compare the
# state variables between Step 0 and 1.
#

set -e

in=$(mktemp)
s0=$(mktemp)
s1=$(mktemp)

cat > $in

ll=$(wc -l $in | awk '{print $1}')
l0=$(egrep -n 'Step 0' $in | awk -F ':' '{print $1}')
l1=$(egrep -n 'Step 1' $in | awk -F ':' '{print $1}')
# printf "found l0 = %d, l1 = %s, ll = %d\n" $l0 $l1 $ll

head -n $l1 $in | tail -n $(($l1-$l0-1)) > $s0
tail -n $(($ll-$l1-1)) $in > $s1

vimdiff $s0 $s1

rm $in
rm $s0
rm $s1
