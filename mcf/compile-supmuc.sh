#! /bin/bash

set +e
set +u
source /etc/profile
source /etc/profile.d/modules.sh

module unload mpi.ibm
module load mpi.mpich2/1.4/gcc
fortrancomp=gfortran
suffix=4.4.5
PREFIX=${HOME}/mcf-${fortrancomp}${suffix}

./configure --prefix=${PREFIX} FC=gfortran MPIFC=mpif90 \
    LDFLAGS=-L${PREFIX}/lib/ FCFLAGS="-I${PREFIX}/include -g" \
    MAKEDEPF90=${PREFIX}/bin/makedepf90 

make -j 8
