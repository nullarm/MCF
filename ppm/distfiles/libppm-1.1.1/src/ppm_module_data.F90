#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

      !-------------------------------------------------------------------------
      !  Module       :                ppm_module_data
      !-------------------------------------------------------------------------
      !
      !  Purpose      :  Declare global types and variables.
      !                
      !  Remarks      :
      !
      !  References   :
      !
      !  Revisions    :
      !-------------------------------------------------------------------------
      !  $Log: ppm_module_data.f,v $
      !  Revision 1.5  2006/07/04 15:44:43  ivos
      !  Added missing comment.
      !
      !  Revision 1.4  2006/02/03 09:34:02  ivos
      !  Fixed bug 00015: ppm_subs_bc was only allocated and stored for the
      !  local subs in topo_store. Several mapping routines however need the
      !  info about all (global) subs.
      !  Changed subs_bc to hold now the GLOBAL subid and adjusted all
      !  occurrences.
      !
      !  Revision 1.3  2004/07/26 11:48:10  ivos
      !  Fixes to make it compile.
      !
      !  Revision 1.2  2004/07/26 10:52:36  hiebers
      !  added ppm_pi
      !
      !  Revision 1.1  2004/07/26 07:28:16  ivos
      !  Initial implementation. Originated from splitting the old ppm
      !  modules.
      !
      !-------------------------------------------------------------------------
      !  Perallel Particle Mesh Library (PPM)
      !  Institute of Computational Science
      !  ETH Zentrum, Hirschengraben 84
      !  CH-8092 Zurich, Switzerland
      !-------------------------------------------------------------------------

      MODULE ppm_module_data

         !----------------------------------------------------------------------
         !  Global TYPEs
         !----------------------------------------------------------------------

         !----------------------------------------------------------------------
         !  Header file for global parameters
         !----------------------------------------------------------------------
         INCLUDE 'ppm_param.h'

         !----------------------------------------------------------------------
         !  buffers for communication
         !----------------------------------------------------------------------
         REAL(ppm_kind_double), DIMENSION(  :), POINTER :: &
     &      ppm_sendbufferd=>null(), & ! send buffer for particles (double)
     &      ppm_recvbufferd=>null()    ! recv buffer for particles (double)

         REAL(ppm_kind_single), DIMENSION(  :), POINTER :: &
     &      ppm_sendbuffers=>null(), & ! send buffer for particles (single)
     &      ppm_recvbuffers=>null()    ! recv buffer for particles (single)

         INTEGER              , DIMENSION(:,:), POINTER :: &
     &      ppm_ghosthack=>null()      ! invert map of ghost for symmetry

         INTEGER              , DIMENSION(  :), POINTER :: &
     &      ppm_psendbuffer=>null(), & ! pointer to particles within the send buffer
     &      ppm_precvbuffer=>null()    ! pointer to particles within the recv buffer
                               ! both in terms of particle NOT the actual
                               ! position in the buffer

         INTEGER                                        :: &
     &      ppm_nsendbuffer, & ! the size of the send buffer 
     &      ppm_nrecvbuffer    ! the size of the recv buffer
                               ! both in terms of entries in the buffer NOT
                               ! the number of particles 

         INTEGER                                        :: &
     &      ppm_buffer_set     ! the total number of particle fields packed in
                               ! the send buffer, ie. xp, vp is two sets

         ! the original on-processor particle IDs in the order in which
         ! they are in the sendbuffer. Used to push additional particle
         ! data on the buffer in the correct order.
         INTEGER              , DIMENSION(  :), POINTER :: ppm_buffer2part=>null()
         INTEGER              , DIMENSION(  :), POINTER :: ppm_buffer_type=>null()
         INTEGER              , DIMENSION(  :), POINTER :: ppm_buffer_dim=>null()

         !----------------------------------------------------------------------
         !  pointers to the subdomains
         !----------------------------------------------------------------------
         ! number of subs on the current processor. index: topoid
         INTEGER              , DIMENSION(:    ), POINTER :: ppm_nsublist=>null()
         ! list of subs of the current processor. 1st index: local sub
         ! number. 2nd: topoid
         INTEGER              , DIMENSION(:,:  ), POINTER :: ppm_isublist=>null()
         ! total number of subs on all processors. index: topoid
         INTEGER              , DIMENSION(:    ), POINTER :: ppm_nsubs =>null()
         ! extensions of all subs (double and single prec). 1st index:
         ! x,y,(z). 2nd: sub-ID. 3rd: topoid
         REAL(ppm_kind_double), DIMENSION(:,:,:), POINTER :: &
     &      ppm_min_subd=>null(),ppm_max_subd=>null()
         REAL(ppm_kind_single), DIMENSION(:,:,:), POINTER :: &
     &      ppm_min_subs=>null(),ppm_max_subs=>null()
         ! boundary conditions on a sub:
         !    west  : 1
         !    east  : 2
         !    south : 3
         !    north : 4
         !    bottom: 5
         !    top   : 6
         ! index 1: the state of the 4 or 6 faces in 2 and 3 D
         !    value: 0 the face is internal
         !    value: 1 otherwise
         ! index 2: the sub id (GLOBAL of ALL the subs!)
         ! index 3: the topoid
         INTEGER              , DIMENSION(:,:,:), POINTER :: ppm_subs_bc=>null()

         !----------------------------------------------------------------------
         !  topology
         !----------------------------------------------------------------------
         INTEGER , DIMENSION(:,:), POINTER :: ppm_subs2proc=>null()
         ! highest internal topology number (= #of topologies)
         INTEGER                           :: ppm_max_topoid
         ! ID of the current particle topology (in internal numbering)
         INTEGER                           :: ppm_topoid
         ! ID of the current field topology (in internal numbering)
         INTEGER                           :: ppm_field_topoid
         ! user-numbering of the topologies
         INTEGER , DIMENSION(:), POINTER   :: ppm_user_topoid=>null()
         ! inverse list: internal numbers indexed by user numbering
         INTEGER , DIMENSION(:), POINTER   :: ppm_internal_topoid=>null()
         ! list of neighboring subs of all local subs. 
         !    index 1: neighbor index
         !    index 2: sub id (local index, not global ID!)
         !    index 3: topoid
         INTEGER , DIMENSION(:,:,:), POINTER :: ppm_ineighsubs=>null()
         ! number of neighboring subs of all local subs. 
         !    index 1: sub id (local index, not global ID!)
         !    index 2: topoid
         INTEGER , DIMENSION(:,:), POINTER :: ppm_nneighsubs=>null()
         ! list of neighboring processors. Index 1: neighbor index, index
         ! 2: topoid
         INTEGER , DIMENSION(:,:), POINTER :: ppm_ineighlist=>null()
         ! number of neighboring processors. Index: topoid
         INTEGER , DIMENSION(:  ), POINTER :: ppm_nneighlist=>null()
         ! has optimal communication sequence already been determined for a
         ! certain topology (index: topoid)
         LOGICAL , DIMENSION(:  ), POINTER :: ppm_isoptimized=>null()
         ! number of communication rounds needed for partial mapping
         ! (index: topoid)
         INTEGER , DIMENSION(:  ), POINTER :: ppm_ncommseq=>null()
         ! optimal communication sequence for this processor. 1st index:
         ! communication round, 2nd index: topoid
         INTEGER , DIMENSION(:,:), POINTER :: ppm_icommseq=>null()

         ! boundary conditions for the topology
         !   first  index: 1-6 each of the faces
         !   second index: topoid
         INTEGER              , DIMENSION(:,:  ), POINTER :: ppm_bcdef=>null()
         ! physical extend of the topology 
         !   first  index: ppm_dim 
         !   second index: topoid
         REAL(ppm_kind_single), DIMENSION(:,:  ), POINTER :: ppm_min_physs=>null(), &
        &                                                    ppm_max_physs=>null()
         REAL(ppm_kind_double), DIMENSION(:,:  ), POINTER :: ppm_min_physd=>null(), &
        &                                                    ppm_max_physd=>null()

         !----------------------------------------------------------------------
         !  mapping
         !----------------------------------------------------------------------
         INTEGER                        :: ppm_map_type 
         INTEGER                        :: ppm_nsendlist,ppm_nrecvlist
         INTEGER, DIMENSION(:), POINTER :: ppm_isendlist=>null(),ppm_irecvlist=>null()

         !----------------------------------------------------------------------
         !  Precision
         !----------------------------------------------------------------------
         INTEGER :: ppm_kind
         INTEGER :: ppm_mpi_kind

         !----------------------------------------------------------------------
         !  Dimensionality
         !----------------------------------------------------------------------
         INTEGER :: ppm_dim

         !----------------------------------------------------------------------
         !  Debugging
         !----------------------------------------------------------------------
         INTEGER :: ppm_debug = 0

         !----------------------------------------------------------------------
         !  Has ppm_init been called?
         !----------------------------------------------------------------------
         LOGICAL :: ppm_initialized = .FALSE.

         !----------------------------------------------------------------------
         !  parallel variables
         !----------------------------------------------------------------------
         INTEGER :: ppm_nproc
         INTEGER :: ppm_rank
         INTEGER :: ppm_comm
         ! relative speeds of the processors (for load balancing)
         REAL(ppm_kind_double), DIMENSION(:), POINTER :: ppm_proc_speed=>null()

         !----------------------------------------------------------------------
         !  Numerical tolerance. Differences smaller than this are considered
         !  zero.
         !----------------------------------------------------------------------
         REAL(ppm_kind_double) :: ppm_myepsd
         REAL(ppm_kind_single) :: ppm_myepss

         !----------------------------------------------------------------------
         !  Constants (computed in ppm_init)
         !----------------------------------------------------------------------
         REAL(ppm_kind_double) :: ppm_pi_d
         REAL(ppm_kind_single) :: ppm_pi_s

         !----------------------------------------------------------------------
         !  I/O Units
         !----------------------------------------------------------------------
         INTEGER               :: ppm_stdout = 6
         INTEGER               :: ppm_stderr = 0
         INTEGER               :: ppm_logfile = -1
         
      END MODULE ppm_module_data
