#!/usr/bin/env bash
#
# Results may be extracted from the .logs/<start_time> directory
# by running:
#
#   % egrep -E -h '^.*RESULT' * | sed 's/^.*RESULT //'

set -o pipefail

###  PARAMETERS  ###########################################

# A larger number of parameter configurations with varying total
# number of nodes.
mixed_params=(\
    "1,1" \
    "2,1" "1,2" \
    "3,1" "2,2" "1,3" \
    "4,1" "3,2" "2,3" "1,4" \
    "5,1" "4,2" "3,3" "2,4" "1,5" \
    "6,1" "5,2" "4,3" "3,4" "2,5" "1,6" \
    "7,1" "6,2" "5,3" "4,4" "3,5" "2,6" "1,7" \
    "8,1" "7,2" "6,3" "5,4" "4,5" "3,6" "2,7" "1,8" \
    "9,1" "8,2" "7,3" "6,4" "5,5" "4,6" "3,7" "2,8" "1,9" \
    "10,1" "9,2" "8,3" "7,4" "6,5" "5,6" "4,7" "3,8" "2,9" "1,10" \
)

# Small parameters, mostly for testing the benchmark script
small_params=(\
    "1,1" \
    "2,1" "1,2" \
    "3,1" "2,2" "1,3" \
)

# Parameters for OM(1) with different numbers of generals
diag_params=("1,1" "2,2" "3,3" "4,4" "5,5" "6,6" "7,7" "8,8" "9,9" "10,10")


###  EDIT ME  ##############################################

# Specify which parameter set to use
param_set="${small_params[@]}"

# Specify how many concurrent jobs to run
CONCURRENT_LIMIT=${CONCURRENT_LIMIT:-1}
export CONCURRENT_LIMIT  # exported for concurrent.lib.sh

# specify timeout
TIME_LIMIT=${TIME_LIMIT:-"1h"}


###  JOB COMMANDS  #########################################

# source 'concurrent.lib.sh' from the current directory
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/concurrent.lib.sh"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/common.lib.sh"

SAL_SMC=$(resolve_prog sal-smc)          # SAL symbolic model checker
SAL_BMC=$(resolve_prog sal-inf-bmc)  # SAL inf-state bounded model checker

CS="\033[0;31m"  # red
CG="\033[0;32m"  # green
CE="\033[0m"     # no color

# run SAL symbolic model checker with specific options, redirect output to a
# temporary file, match the "total execution time" line and extract run time
sal_smc_cmd() {
    local model=$1
    local par=$2
    local tmp=$(mktemp sal_cmd.XXXXXX -ut)
    local output=""

    timeout $TIME_LIMIT \
        ${SAL_SMC} -v 1 \
        --disable-traceability \
        --backward \
        --enable-slicer \
        --enable-dynamic-reorder \
        "${model}{$par}" vaa \
        2>&1 | tee "$tmp"
    local code=${PIPESTATUS[0]}

    # output run time next to job status
    if [ $code -eq 124 ]; then
        # timeout
        echo "RESULT $par, timeout"
        echo "timeout" >&3
    elif [ $code -ne 0 ]; then
        output="fail: exit status $code"
        echo $output
        echo $output >&3
    else
        # normal return
        cat "$tmp" | awk "/total exec/ {print \"RESULT $par,\", \$4}"
        cat "$tmp" | awk "/total exec/ {print \"proved in \", \$4, \"s\"}" >&3
    fi
    rm "$tmp"
    return $code
}

sal_bmc_cmd() {
    local model=$1
    local par=$2
    local tmp=$(mktemp sal_cmd.XXXXXX -ut)
    local output=""

    timeout $TIME_LIMIT \
        ./runproof.sh $model -par "{$par}" \
        2>&1 | tee "$tmp"
    local code=${PIPESTATUS[0]}  # capture exit status of timeout + runproof

    # output run time next to job status
    if [ $code -eq 124 ]; then
        echo "RESULT $par, timeout"
        echo "timeout" >&3
    elif [ $code -ne 0 ]; then
        output="fail: exit status $code"
        echo $output
        echo $output >&3
    else
        cat "$tmp" | awk "/total time ellapsed/ {print \"RESULT $par,\", \$4}"
        cat "$tmp" | awk "/total time ellapsed/ {print \"proved in \", \$4, \"s\"}" >&3
    fi
    rm "$tmp"
    return $code
}

main_rushby() {
    local params=($1)
    local jobs=( )

    for p in "${params[@]}"; do
        jobs=("${jobs[@]}" - "(sal-smc) prove 'vaa' for om1{$p}" \
              sal_smc_cmd "om1" "$p")
    done

    concurrent "${jobs[@]}"
}

main_affirm() {
    local params=($1)
    local jobs=( )

    for p in "${params[@]}"; do
        jobs=("${jobs[@]}" - \
              "(sal-inf-bmc) prove 'validity' & 'agreement' for om1_cal{$p}" \
              sal_bmc_cmd "om1_cal.sal" "$p")
    done

    concurrent "${jobs[@]}"
}


# param_set is specified above
arg1="${1:-}"
case $arg1 in
    "rushby")
        main_rushby "${param_set[@]}"
        ;;
    "affirm")
        main_affirm "${param_set[@]}"
        ;;
    *)
        echo "unknown target '$arg1', choose either 'rushby' or 'affirm'"
        exit 1
        ;;
esac
