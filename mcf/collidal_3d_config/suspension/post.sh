#!/bin/bash

set -e
set -u

root=$(pwd)
for d in $(ls -d NUM*); do
    cd ${d}
    for c in $(ls -f colloid/mcf*); do
	awk '{print $1, $3, $2, 0.95}' ${c}
	printf "\n"
    done > punto.dat
    cd "${root}"
done

# punto -bg white -r -z 1:2:3:4 -B 0:0:0:8:8:8 punto.dat
