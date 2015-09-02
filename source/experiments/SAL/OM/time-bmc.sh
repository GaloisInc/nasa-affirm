#!/bin/bash

#PARAMS="1,1 1,2 2,1 2,2 2,3 3,2"
PARAMS="1,1 2,1 3,1 1,2 2,2 3,2 3,3"
DEPTH=50
#SAL=sal-inf-bmc
SAL=sal-bmc

for p in $PARAMS; do
    echo "------------"
    echo "(n,k) = ($p)"
    (time $SAL --disable-traceability -d $DEPTH "om1_cal_v3{$p}" \
          not_all_done) 2>&1 \
      | grep real \
      | awk '{print $2}'
done
