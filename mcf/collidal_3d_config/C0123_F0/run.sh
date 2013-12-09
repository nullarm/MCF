#!/bin/bash

set -e
set -u

mkdir -p particles boundary colloid
mpirun -np 8 ../../src/mcf

