#!/usr/bin/env bash

set -e -o pipefail

# source 'concurrent.lib.sh' from the current directory
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/concurrent.lib.sh"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/common.lib.sh"

SAL_SMC=$(resolve_prog sal-smc)

###  PARAMETERS  ###########################################

mixed_params=(\
    "1,1" \
    "2,1" "1,2" \
    "3,1" "2,2" "1,3" \
    "4,1" "3,2" "2,3" "1,4" \
    "5,1" "4,2" "3,3" "2,4" "1,5" \
    "6,1" "5,2" "4,3" "3,4" "2,5" "1,6" \
    "7,1" "6,2" "5,3" "4,4" "3,5" "2,6" "1,7" \
)

small_params=(\
    "1,1" \
    "2,1" "1,2" \
    "3,1" "2,2" "1,3" \
)

diag_params=("1,1" "2,2" "3,3" "4,4" "5,5")



###  JOB COMMANDS  #########################################

# run SAL symbolic model checker with specific options, redirect output to a
# temporary file, match the "total execution time" line and extract run time
sal_cmd() {
    local model=$1
    local tmp=$(mktemp sal_cmd.XXXXXX -ut)

    ${SAL_SMC} -v 1 \
        --disable-traceability \
        --enable-slicer \
        --enable-dynamic-reorder \
        "$model" \
        vaa \
        2>&1 | tee "$tmp"

    # output run time next to job status
    cat "$tmp" | awk "/total exec/ {print \"proved. time\", \$4, \"s\"}" >&3
    rm "$tmp"
}

main() {
    local params=("${mixed_params[@]}")
    local jobs=( )

    for p in "${params[@]}"; do
        jobs=("${jobs[@]}" - "prove 'vaa' for om1{$p}" sal_cmd "om1{$p}")
    done

    concurrent "${jobs[@]}"
}

main



# OLD sequential run
#
# for p in "${params[@]}"; do
#     printf "proving $p\n"
#     $sal_cmd "om1{$p}" vaa 2>&1 | \
#         awk "\
#              /^proved/          {print} \
#              /^total execution/ {print \"size $p time\", \$4} \
#             "
# done

# Example from concurrent.lib.sh
#
#    local args=(
#        - "Creating VM"                                         create_vm    3.0
#        - "Creating ramdisk"                                    my_sleep     0.1
#        - "Enabling swap"                                       my_sleep     0.1
#
#        --require "Creating VM"
#        --before  "Creating ramdisk"
#        --before  "Enabling swap"
#    )
