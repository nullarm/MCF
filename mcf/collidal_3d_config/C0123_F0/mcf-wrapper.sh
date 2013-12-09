#!/bin/bash

function githead() {
    git --git-dir=${src}/.git --work-tree=${src} rev-parse HEAD
}

function cpreplace() {
    local varfile=$1
    local dname=$2
    mkdir -p ${dname}/particles ${dname}/boundary ${dname}/colloid
    shift 2
    for f in $*; do
	./vars.awk ${varfile} ${f} > ${dname}/${f}
    done
    cp ${varfile} ${dname}/vars.mcf
}

function var2dirname() {
    awk -v FS="=" -v ORS="" 'NF{$1=$1; gsub(/ /, ""); print}'  $1
}

mcf=/scratch/work/MCF/mcf/src/mcf
src=/scratch/work/MCF/

for b in $(seq 4.5 0.25 6.0); do
    echo "COLB=$b" > vars.mcf.${b}
    dname=$(var2dirname vars.mcf.${b})
    cpreplace vars.mcf.${b} ${dname} ctrl.mcf  io_config.mcf  physics_config.mcf
    cd ${dname}
    githead > git.commit.id
    mpirun -np 1 ${mcf} &
    cd ../
done

