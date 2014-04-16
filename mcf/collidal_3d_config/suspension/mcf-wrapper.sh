#!/bin/bash

function githead() {
    git --git-dir=${src}/.git --work-tree=${src} rev-parse HEAD
}

function cpreplace() {
    local varfile=$1
    local dname=$2
    if [ $(whoami) = "lu79buz2" ]; then
	mkdir -p ${SCRATCH}/${dname}
	ln -s ${SCRATCH}/${dname} ${dname} 
    fi
    mkdir -p ${dname}/particles ${dname}/boundary ${dname}/colloid ${dname}/restart
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
	    -e "s,M4_NP,${nproc},g" \
	    -e "s,M4_JOB_NAME,${dname},g" run.m4.sh \
	     > ${dname}/run.sh
	llsubmit ${dname}/run.sh
    elif [ $(hostname) = "kana" ]; then
	local root=$(pwd)
	cd ${dname}
	mpirun -np 8 nice -n 15 ${mcf}
	cd ${root}
    fi
}

if [ $(whoami) = "lu79buz2" ]; then
    mcf=~/work/MCF/mcf/src/mcf
    src=~/work/MCF/
    nproc=32
elif [ $(hostname) = "kana" ]; then 
    mcf=/scratch/work/MCF/mcf/src/mcf
    src=/scratch/work/MCF/
fi

n=1
SIZE=16.0
NUM_PART=80
# generate a grid of colloid
#awk --lint=fatal -v r=0.95 -v step=1.95 -v size=${SIZE} -f simplegrid.awk   > xyz.tmp
awk -v step=0.95 -v r=0.9 -v nfail=1000 -v ncol=10000 -v size=${SIZE} -f randomgrid.awk       > xyz.tmp

NUM_COLLOID=$(wc -l < xyz.tmp)
awk -f xyz2col.awk xyz.tmp > col.tmp
# generate a physics_config.mcf with all colloids in
awk -v tmpfile=col.tmp -v line=COLL_SHAPE -f line2file.awk physics_config.m4 > physics_config.mcf

echo "NUM_COLLOID=${NUM_COLLOID}" > vars.mcf.${n}
echo "DOMAIN_SIZE=${SIZE}" >> vars.mcf.${n}
echo "NUM_PART=${NUM_PART}" >> vars.mcf.${n}

dname=$(var2dirname vars.mcf.${n})
cpreplace vars.mcf.${n} ${dname} ctrl.mcf  io_config.mcf  physics_config.mcf
githead > ${dname}/git.commit.id
rundispatch
