#! /bin/bash

if [ -z "$1" ]; then
    dirname=.
else
    dirname=$1
fi

rm punto_particles_marker.dat

perl  ~/MCF/mcf/scripts/particles_3d_marker.perl ${dirname}

