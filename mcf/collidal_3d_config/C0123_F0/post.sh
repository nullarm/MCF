#!/bin/bash

set -e
set -u

for c in $(ls colloid/mcf*); do
    awk '{print $1, $2, $3}' ${c}
    printf "\n"
done > punto.dat
