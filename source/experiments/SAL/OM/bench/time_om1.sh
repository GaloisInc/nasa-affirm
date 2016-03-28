#!/usr/bin/env bash

set -e -o pipefail

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

# Specify which parameter set to use
param_set="${small_params[@]}"


###  JOB COMMANDS  #########################################

# source 'concurrent.lib.sh' from the current directory
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/concurrent.lib.sh"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/common.lib.sh"

SAL_SMC=$(resolve_prog sal-smc)          # SAL symbolic model checker
SAL_BMC=$(resolve_prog sal-inf-bmc)  # SAL inf-state bounded model checker

# run SAL symbolic model checker with specific options, redirect output to a
# temporary file, match the "total execution time" line and extract run time
sal_smc_cmd() {
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

sal_bmc_cmd() {
    local model=$1
    local par=$2
    local tmp=$(mktemp sal_cmd.XXXXXX -ut)

    ./runproof.sh $model -par $par \
        2>&1 | tee "$tmp"
    local code=$?

    # output run time next to job status
    cat "$tmp" | awk "/total time ellapsed/ {print \"proved. time\", \$4, \"s\"}" >&3
    rm "$tmp"
    exit $code
}

main_rushby() {
    local params=($1)
    local jobs=( )

    for p in "${params[@]}"; do
        jobs=("${jobs[@]}" - "(sal-smc) prove 'vaa' for om1{$p}" \
              sal_smc_cmd "om1{$p}")
    done

    concurrent "${jobs[@]}"
}

main_affirm() {
    local params=($1)
    local jobs=( )

    for p in "${params[@]}"; do
        jobs=("${jobs[@]}" - \
              "(sal-inf-bmc) prove 'validity' & 'agreement' for om1_cal{$p}" \
              sal_bmc_cmd "om1_cal.sal" "{$p}")
    done

    concurrent "${jobs[@]}"
}

# param_set is specified above
#main_smc "${param_set[@]}"
main_affirm "${param_set[@]}"

