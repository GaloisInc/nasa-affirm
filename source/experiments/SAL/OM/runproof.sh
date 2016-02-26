#!/bin/bash

# set -e -u -x

# BSD License
# Copyright (c) 2008, Lee Pike (Galois, Inc.) leepike [at] galois.com
# Copyright (c) 2015, Benjamin Jones (Galois, Inc.) <bjones@galois.com>
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#  * Neither the name of the <organization> nor the
#    names of its contributors may be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY <copyright holder> ``AS IS'' AND ANY
#  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#  DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
#  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
#  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
#  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --- USAGE ---
# Takes a SAL file, and runs all the proof commands listed in that file in a
# form such as

# %   sal-smc file.sal property1
# or
# % sal-bmc -i -d 1 fail.sal property2
# etc.

# There can only be one '%' (the comment indicator in SAL) preceding the proof
# directive.  An arbitrary amount of whitespace may follow the '%' before the
# directive.

# Proof commands can be listed anywhere -- say all at the top, the bottom, or
# sitting right next to the corresponding theorems.

# -- EXAMPLES ---
# runproof.sh --help : help

# runproof.sh file.sal : outputs to stdout the results of proving file.sal.

# runproof.sh file.sal -p proof_file : outputs to proof_file the results of
# proving file.sal.

# runproof.sh file.sal -p proof_file -e err_file : outputs to proof_file the
# results of proving file.sal and outputs to err_file the standard error from
# SAL.

# --- ARGS ---
# The script takes three options:

# -h --help : displays help (No help exists; please read these comments).

# -p --proof : the optional file to which the output of running the proofs
# should be placed.  Output is a list of proof commands and either 'proved' or
# a counterexample.

# -e --error : the optional file to which standard out is placed.  For example,
# parser problems are placed here.
#
# -c --color : color failed proof checks in red
#

# WARNING: This script overwrites proof and error output files if they already
# exist.

SCRIPT=$0

function usage {
  echo "usage: $SCRIPT sal_file [[-p proof_out.prf] [-e error_out.err] [-c] [-h]]"
  exit 0
}

function hlp {
  echo "help: to do.  Please read the scripts comments."
  usage
}

# If the number of args is less than 1, then exit on error.
if [ $# \< "1" ]; then
  usage
fi

function valid {
   echo "$1 is not a valid SAL file"
   usage
}

# If the first arg is the help flag, display help info.
if [ $1 == "--help" ] || [ $1 == "-h" ]; then
  hlp
  usage
fi

# If the SAL file doesn't exist, exit on error.
if [ ! -e $1 ]; then
  valid $1
fi

SALFILE=$1

# default 'color start' and 'color end' commands
CS=""
CG=""
CE=""

# While there is more than one argument (the SAL file is always the first argument),
# get the remaining arguments.
while [ $# \> "1" ]; do
    case $2 in
        -p | --proof )          shift # Shift past the flag.
                                PRFFILE=$2
                                ;;
        -e | --error )          shift # Shift past the flag.
                                ERRFILE=$2
                                ;;
        -c | --color )          CS="\033[0;31m"  # red
                                CG="\033[0;32m"  # green
                                CE="\033[0m"     # no color
                                ;;
        * )                     shift # Shift past the flag.
                                ;;
    esac
    shift
done

SALOPT=$(egrep -e '^% runproof: ' $SALFILE | tail -1 | cut -d ' ' -f 3-)

function cmd {
    IFS=$'\n'
    declare -a lines=( $(egrep -e "^%[ ]*sal-" $SALFILE | sed "s/^%[ ]*//") )
    unset IFS

    printf "\n** Found %d proof commands\n\n" ${#lines[@]}
    local pass=0
    local fail=0
    for ((i=0; i < ${#lines[@]}; i++))
    do
        local salcmd=${lines[$i]/sal-inf-bmc/sal-inf-bmc $SALOPT}
        printf "[proving]: %s\n" "$salcmd"
        exec $salcmd 2>&1 |\
            awk '/failed/ {print "'$CS'" $0 "'$CE'"} /proved/ {print $0}'
        if [ ${PIPESTATUS[0]} -ne 0 ]; then
            fail=$(($fail+1))
        else
            pass=$(($pass+1))
        fi
    done

    printf "\n"
    printf "** ${CG}Proofs OK %d${CE} / ${CS}Proofs FAILED %d${CE} **\n" $pass $fail
    printf "\n"
}

# Write out to standard out and err to standard error.
if [ -z ${PRFFILE:-} ] && [ -z ${ERRFILE:-} ]; then
 cmd
# Write out to file ${PRFFILE:-} and err to standard error.
elif [ -n ${PRFFILE:-} ] && [ -z ${ERRFILE:-} ]; then
 cmd > ${PRFFILE:-}
# Write out to standard out and err to file ${ERRFILE:-}.
elif [ -z ${PRFFILE:-} ] && [ -n ${ERRFILE:-} ]; then
 cmd &> ${ERRFILE:-}
# Write out to file ${PRFFILE:-} and err to file ${ERRFILE:-}.
elif [ -n ${PRFFILE:-} ] && [ -n ${ERRFILE:-} ]; then
 (cmd > ${PRFFILE:-}) &> ${ERRFILE:-}
fi
