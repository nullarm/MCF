      SUBROUTINE colloid_compute_magnetism_moment(this, dt, stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_moment
        !----------------------------------------------------
        !
        ! Purpose     : Compute the magnetism moment
        !               using the rotating frequency of 
        !               the magnetic field.
        !               
        !
        ! Reference   :
        !
        ! Remark      : 
        !
        ! Revision    : V0.1  26.06.2013, original version.
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
        
        !----------------------------------------------------
        ! Arguments
        !
        ! this           : an object of Particles Class.
        ! stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(INOUT)            :: this
        REAL(MK), INTENT(IN)                    :: dt
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------

        INTEGER                                 :: stat_info_sub
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        
        CALL colloid_compute_magnetism_rotation_vector(this,dt,stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
           "computing magnetism rotation vector failed!"
           stat_info = -1
           GOTO 9999
        END IF

        CALL colloid_compute_magnetism_rotation_matrix(this,stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
                "computing magnetism rotation matrix failed!"
           stat_info = -1
           GOTO 9999
        END IF
        
        this%cc_magnet_mom(1:3) = &
             MATMUL(this%cc_magnet_rot_matrix(1:3,1:3),this%cc_magnet_mom(1:3))
        
        !PRINT *, "mom: ", this%cc_magnet_mom(1:3)

        CALL colloid_compute_magnetism_accumulation_matrix(this,stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
           "computing magnetism accumulation matrix failed!"
           stat_info = -1
           GOTO 9999
        END IF
        
        CALL colloid_compute_magnetism_accumulation_vector(this,stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
                "computing magnetism accumulation vector failed!"
           stat_info = -1
           GOTO 9999
        END IF

        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_compute_magnetism_moment
      
      
