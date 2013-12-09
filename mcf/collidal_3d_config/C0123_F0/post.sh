#!/bin/bash

set -e
set -u

for c in $(ls colloid/mcf*); do
    awk '{print $1, $2, $3, 1.0}' ${c}
    printf "\n"
done > punto.dat


# punto -bg white -r -z 1:2:3:4 punto.dat 
