      SUBROUTINE colloid_compute_magnetism_accumulation_vector(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_accumulation_vector
        !----------------------------------------------------
        !
        ! Purpose     : Compute the accumulative rotation vector
        !
        ! Remark      : 
        !
        ! Reference  : Chen et. al. 2006, physics of fluids
        !              wikipedia
        !
        ! Revision   : V0.1  26.06.2013, original.
        !
        !----------------------------------------------------
        ! Author      : Xin Bian
        ! Contact     : xin.bian@aer.mw.tum.de
        !
        ! Dr. Marco Ellero's Emmy Noether Group,
        ! Prof. Dr. N. Adams' Chair of Aerodynamics,
        ! Faculty of Mechanical Engineering,
        ! Technische Universitaet Muenchen, Germany.
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(INOUT)    :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        INTEGER                         :: stat_info_sub
        REAL(MK),DIMENSION(3)           :: axis
        REAL(MK)                        :: len, phi
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        
        CALL tool_rotation_vector(this%tool, &
             3,this%cc_magnet_acc_rot_matrix(1:3,1:3),&
             axis(1:3),phi,stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
                "Using tool_rotation_vector failed! "
           stat_info = -1
           GOTO 9999
        END IF

        !----------------------------------------------
        ! Normalize roation vector at this time step.
        !----------------------------------------------
              
        len = SQRT(DOT_PRODUCT(axis(1:3),axis(1:3)))
        
        IF ( len < mcf_machine_zero ) THEN
           
           axis(1:3) = 0.0_MK
           axis(2)   = 1.0_MK
           phi       = 0.0_MK
           
        ELSE
           
           axis(1:3) = axis(1:3) / len
           
        END IF
      
        
        this%cc_magnet_acc_rot_vector(1:3) = axis(1:3)
        this%cc_magnet_acc_rot_vector(4)   = phi
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_compute_magnetism_accumulation_vector
      
