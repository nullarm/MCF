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

function rundispatch() {
    if [ $(whoami) = "lu79buz2" ]; then
	local p=$(pwd)/${dname}
	sed -e "s,M4_COMMAND,${mcf},g" \
	    -e "s,M4_DNAME,${p},g" \
	    -e "s,M4_JOB_NAME,${dname},g" run.m4.sh \
	     > ${dname}/run.sh
	#llsubmit ${dname}/run.sh
    else
	cd ${dname}
	mpirun -np 1 ${mcf} &
	cd ../
    fi
}

if [ $(whoami) = "lu79buz2" ]; then
    mcf=~/work/MCF/mcf/src/mcf
    src=~/work/MCF/
else
    mcf=/scratch/work/MCF/mcf/src/mcf
    src=/scratch/work/MCF/
fi

for b in $(seq 4.5 0.25 6.0); do
    echo "COLB=$b" > vars.mcf.${b}
    dname=$(var2dirname vars.mcf.${b})
    cpreplace vars.mcf.${b} ${dname} ctrl.mcf  io_config.mcf  physics_config.mcf
    githead > ${dname}/git.commit.id
    rundispatch
done

