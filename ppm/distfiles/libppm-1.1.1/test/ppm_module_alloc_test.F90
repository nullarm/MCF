module ppm_module_alloc_test

contains 

  subroutine ppm_module_alloc_run
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_MPI
    use mpi, only : MPI_Init, MPI_COMM_WORLD
#endif
    use mod_unit
    use ppm_module_alloc
    USE ppm_module_substart
    USE ppm_module_substop
    use ppm_module_data, only : ppm_kind_double, ppm_char, ppm_param_decomp_bisection, &
         ppm_param_assign_internal, ppm_param_bcdef_periodic, ppm_debug, ppm_param_alloc_fit
    use ppm_module_init, only : ppm_init
    implicit none

    integer, parameter :: MK        = ppm_kind_double

#include "ppm_define.h"

    real(MK), dimension(:), allocatable                        :: min_phys,max_phys

    integer, parameter  :: ndim    = 2
    real(MK), parameter :: cutoff = 1.0_MK
    integer, parameter :: tolexp = INT(LOG10(EPSILON(cutoff)))+1
    INTEGER, DIMENSION(2)                   :: ldc
    INTEGER                                 :: iopt

    !> NOTE: fortran 95 features
    real(MK), dimension(:,:), pointer  :: min_sub => null() 

    integer                            :: decomp
    integer                            :: info

    
#ifdef HAVE_MPI
    call unit_init(4)
    call MPI_Init(info)
    call unit_assert_equal("MPI_Init", info, 0)
    call ppm_init(ndim, MK, tolexp, MPI_COMM_WORLD, 0, info)
    call unit_assert_equal("ppm_init", info, 0)
#else 
    call unit_init(3)
    call ppm_init(ndim, MK, tolexp, 0, 0, info)
    call unit_assert_equal("ppm_init", info, 0)
#endif
    ppm_debug = 2
    iopt     = ppm_param_alloc_fit
    ldc = (/2, 1/)

    CALL ppm_alloc(min_sub,ldc,iopt,info)
    call unit_assert_equal("alloc ret code", info, 0)
    call unit_assert_equal("shape of the array", shape(min_sub), ldc)
    call unit_finalize()
  end subroutine ppm_module_alloc_run
end module ppm_module_alloc_test
