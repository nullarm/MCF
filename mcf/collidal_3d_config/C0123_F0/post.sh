#!/bin/bash

set -e
set -u

for d in $(ls -d COLB*); do
    cd ${d}
    for c in $(ls colloid/mcf*); do
	awk '{print $1, $3, $2, 1.0}' ${c}
	printf "\n"
    done > punto.dat
    cd ..
done


# punto -bg white -r -z 1:2:3:4 -B 0:0:0:8:8:8 punto.dat
