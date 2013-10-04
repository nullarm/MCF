#! /bin/bash


make PREFIX=/scratch/prefix-mcf CXX=g++ CC=gcc FC=gfortran MPIFC=/scratch/prefix-mcf/bin/mpif90 \
     BUILD_MAKE_FLAGS=-j1 FCFLAGS='-g -O2' \
     WRKDIR=work-gfortran \
     install_all
