#! /bin/bash

. /etc/profile
. /etc/profile.d/modules.sh

set -e
set -u

module unload mpi.ibm
module load mpi.mpich2/1.4/gcc
fortrancomp=gfortran
suffix=4.4.5

make MPIFC=mpif90 FC=mpif90 CC=mpicc CXX=mpicxx \
     PREFIX=${HOME}/mcf-${fortrancomp}${suffix} \
     BUILD_MAKE_FLAGS=-j16 \
     WRKDIR=${PWD}/work-${fortrancomp}${suffix} \
     install_all_but_mpi
