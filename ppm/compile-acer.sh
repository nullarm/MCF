#! /bin/bash

PREFIX=${HOME}/prefix-ifort-mcf

make PREFIX=${PREFIX} CXX=g++ CC=gcc FC=${PREFIX}/bin/mpif90 MPIFC=${PREFIX}/bin/mpif90 \
     BUILD_MAKE_FLAGS=-j1 FCFLAGS='-g -O0' \
     WRKDIR=work-ifort \
     install_ppm
