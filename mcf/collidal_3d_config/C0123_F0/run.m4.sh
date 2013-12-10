#!/bin/bash
##
## optional: energy policy tags
##
#@ job_type = parallel
#@ island_count = 1
#@ class = micro
#@ node = 1
#@ total_tasks= M4_NP
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

mpiexec -n M4_NP M4_COMMAND
