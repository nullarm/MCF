#!/bin/bash

set -e
set -u

root=$(pwd)
for d in $(ls -d COL*); do
    cd ${d}
    printf "processing: %s\n" ${d} > "/dev/stderr"
    for c in $(ls colloid/mcf*); do
	r1=1
	r2=$(awk -v FS="=" '$1=="COLR"{print $2}' vars.mcf)
	if [ -n "${r2}" ]; then
	    awk -v r1=${r1} -v r2=${r2} 'NR%2==0{print $1, $2, $3, $4, $5, $6, r1} NR%2==1{print $1, $2, $3, $4, $5, $6, r2}' ${c}
	else
	    awk '{print $1, $3, $2, 1.0}' ${c}
	fi
	printf "\n"
    done > punto.dat
    awk '!NF{s=0; print xs-xb, ys-yb, zs-zb, xs-x0, ys-y0, zs-z0, vxs-vxb, vys-vyb, vzs-vzb; next} NF{s++}
    s==2&&NR==2{x0=$1; y0=$2; z0=$3; vx0=$4; vy0=$5; vz0=$5}
    s==1{xs=$1; ys=$2; zs=$3; vxs=$4; vys=$5; vzs=$5}
    s==2{xb=$1; yb=$2; zb=$3; vxb=$4; vyb=$5; vzb=$5}' punto.dat > traj.dat
    cd "${root}"
done

# punto -bg white -r -z 1:3:2:7 -B 0:0:0:8:8:8 COLR9.510e-01COLZ6.046e+00/punto.dat 
