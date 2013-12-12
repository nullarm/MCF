#!/bin/bash

set -e
set -u

for d in $(ls -d COL*); do
    cd ${d}
    printf "processing: %s\n" ${d} > "/dev/stderr"
    for c in $(ls colloid/mcf*); do
	r1=1
	r2=$(awk -v FS="=" '$1=="COLR"{print $2}' vars.mcf)
	if [ -n "${r2}" ]; then
	    awk -v r1=${r1} -v r2=${r2} 'NR%2==0{print $1, $3, $2, r1} NR%2==1{print $1, $3, $2, r2}' ${c}
	else
	    awk '{print $1, $3, $2, 1.0}' ${c}
	fi
	printf "\n"
    done > punto.dat
    cd ..
done

# punto -bg white -r -z 1:2:3:4 -B 0:0:0:8:8:8 punto.dat
