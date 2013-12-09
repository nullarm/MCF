#!/bin/bash

set -e
set -u

for d in $(ls -d COLB*); do
    cd ${d}
    for c in $(ls colloid/mcf*); do
	awk '{print $1, $2, $3, 1.0}' ${c}
	printf "\n"
    done > punto.dat
    cd ..
done


# punto -bg white -r -z 1:2:3:4 punto.dat 
