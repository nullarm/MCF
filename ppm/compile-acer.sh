#! /bin/bash

PREFIX=${HOME}/prefix-gfortran-mcf

make PREFIX=${PREFIX} CXX=g++ CC=gcc FC=${PREFIX}/bin/mpif90 MPIFC=${PREFIX}/bin/mpif90 \
     BUILD_MAKE_FLAGS=-j1 FCFLAGS='-g -O2' \
     WRKDIR=work-gfortran \
     install_ppm
