#! /bin/bash

if [ -z "$1" ]; then
    dirname=.
else
    dirname=$1
fi

rm ${dirname}/punto_particles.dat 

for F in ${dirname}/mcf_*particles*.out; do
    
    printf "%s" $F
    perl  ~/MCF/mcf/scripts/particles_3d.perl $F 'punto_particles.dat'
    printf "\n" >> ${dirname}/punto_particles.dat

done
