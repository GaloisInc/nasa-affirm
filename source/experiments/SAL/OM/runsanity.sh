#!/bin/bash
#
# Run sanity checks for the om1_cal.sal model (INFINITE version).

set -e

for i in $(seq 0 4); do
    sal-inf-bmc --disable-traceability -d 25 'om1_cal{2,2}' "sanity$i"
done
