set term x11 1
r=1
V=8**3 - 8*8*2
Vc=4.0/3.0*pi*r

file(n)=sprintf("../suspension/supermuc-data/NUM_COLLOID%iDOMAIN_SIZE8.0/boundary/mcf_boundary.dat", n)
ti(n)=sprintf("eta = %.3f", Vc*n/V)

set key right
set xlabel "time"
set ylabel "Fx"
plot [1.0:][0:] \
     for [n in "1 5 10 15 20 30"] file(n+0) u 2:3 t ti(n+0)

set key left
set term x11 2
set xlabel "z"
set ylabel "vx"
plot \
     "../suspension/supermuc-data/NUM_COLLOID10DOMAIN_SIZE8.0/particles/mcf_particles00065000.out" u 3:4 t ti(10), \
     "../suspension/supermuc-data/NUM_COLLOID20DOMAIN_SIZE8.0/particles/mcf_particles00060000.out" u 3:4 t ti(20), \
     "../suspension/supermuc-data/NUM_COLLOID30DOMAIN_SIZE8.0/particles/mcf_particles00020000.out" u 3:4 t ti(30)