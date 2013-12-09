#!/bin/bash
##
## optional: energy policy tags
##
#
#@ job_type = MPICH
#@ island_count = 1
#@ class = fat
#@ node = 1
#@ total_tasks= 40
#@ wall_clock_limit = 10:00:00
#@ job_name = M4_JOB_NAME
#@ network.MPI = sn_all,not_shared,us
#@ output = job$(jobid).out
#@ error = job$(jobid).err
#@ notification=always
#@ notify_user= sergej.litvinov@aer.mw.tum.de
#@ queue

. /etc/profile
. /etc/profile.d/modules.sh


module unload mpi.ibm
module load mpi.mpich2/1.4/gcc

cd M4_DNAME

mpiexec -n 40 M4_COMMAND
