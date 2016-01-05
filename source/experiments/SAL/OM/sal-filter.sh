#!/bin/bash
#
# Use munge.sh to filter SAL counter example output. Dividers and transition
# info are left in the output to aid reading.

exec ./munge.sh '===' '---' '\s*\(with ' '\s*\(label ' $* \
    | grep -v 'System Variables' \
    | uniq
