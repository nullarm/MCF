#! /bin/bash

. /etc/profile
. /etc/profile.d/modules.sh

set -e
set -u

module unload mpi.ibm
module load mpi.intel

module unload fortran
module load fortran/intel

fortrancomp=ifort
suffix=
CXX=mpiCC

make MPIFC=mpif90 FC=mpif90 CC=mpicc CXX=${CXX} \
     PREFIX=${HOME}/mcf-${fortrancomp}${suffix} \
     BUILD_MAKE_FLAGS=-j16 \
     WRKDIR=${PWD}/work-${fortrancomp}${suffix} \
     install_all_but_mpi
