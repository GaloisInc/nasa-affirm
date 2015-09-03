#!/bin/bash

MODEL="om1_cal_v4.sal"
THM="vaa"
SAL="sal-bmc -v 1 --disable-traceability"
DEPTHS=$(seq 1 40)

for d in $DEPTHS; do
    #echo "$SAL -d $d $MODEL $THM"
    echo -n "$d  "
    (time $SAL -d $d $MODEL $THM) 2>&1 \
      | grep real \
      | awk '{print $2}'
done
