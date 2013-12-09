      SUBROUTINE particles_compute_mass(this,stat_info) 
        !----------------------------------------------------
        ! Subroutine  : particles_compute_mass
        !----------------------------------------------------
        !
        ! Purpose     : Computing inital mass of particles,
        !               done on each local process.
        !      
        ! Reference   :
        !
        ! Remark      :
        !               shape = 1: 2D disk / 3D sphere
        !               For mass, it is trival.
        !               For moment of inertia, wikipedia
        !               gives a list.
        !
        !               shape = 2: 2D ellipse / 3D ellipsoid
        !               For moment inertia of ellipse,
        !               see http://www.eformulae.com/
        !               engineering/moment_of_inertia.php
        !               For moment of inertia of ellipsoid,
        !               it is trival, check wikipedia.
        !
        !               shape = 3: 2D star
        !               we calculate its mass and moment
        !               of inertia numerically, i.e.,
        !               take sufficiently thin piece along
        !               radia direction.
        !
        !               shape = 4: 2D / 3D dicolloid, i.e,
        !               two overlapping 2D disks or 3D spheres.
        !               check the report I wrote.
        !
        !
        ! Revisions   : V0.5 Nov. 2, 2013, including multiscale
        !               resolutoin/size/scale.
        !              
        !               V0.4 21.11.2011, including ellipsoid
        !               and dicollod.
        !
        !               V0.3 17.12.2009, including mass moment
        !               inertia of ellipse.
        !
        !               V0.2 05.10.2009, including mass moment
        !               inertia of disk/sphere.
        !
        !               V0.1 23.07.2009, original version.
        !
        !----------------------------------------------------
        ! Author       : Xin Bian
        ! Contact      : xin.bian@aer.mw.tum.de
        !
        ! Dr. Marco Ellero's Emmy Noether Group,
        ! Prof. Dr. N. Adams' Chair of Aerodynamics,
        ! Faculty of Mechanical Engineering,
        ! Technische Universitaet Muenchen, Germany.
        !----------------------------------------------------
        
        !----------------------------------------------------
        ! Arguments :
        !----------------------------------------------------        
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)		        :: stat_info
        
        !----------------------------------------------------
        ! Local variables start here :
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: multiscale
        REAL(MK), DIMENSION(:), POINTER         :: min_phys
        REAL(MK), DIMENSION(:), POINTER         :: max_phys
        
        !----------------------------------------------------
        ! New variables
        !----------------------------------------------------

        INTEGER                                 :: chi_level
        REAL(MK)                                :: m0, m1, m2
        REAL(MK)                                :: height, m_increment
        REAL(MK)                                :: psi
        INTEGER                                 :: multiscale_shape
        INTEGER                                 :: i, j
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(min_phys)
        NULLIFY(max_phys)
        
        !----------------------------------------------------
        ! Dimension and initial density.
        ! Number of symmetry boundaries.
        ! Number of colloid particle.
        !----------------------------------------------------
        
        multiscale = &
             control_get_multiscale(this%ctrl,stat_info_sub)
        chi_level        = &
             physics_get_chi_level(this%phys,stat_info_sub)
        multiscale_shape = &
             physics_get_multiscale_shape(this%phys,stat_info_sub)
        Call physics_get_min_phys(this%phys,min_phys,stat_info_sub)
        Call physics_get_max_phys(this%phys,max_phys,stat_info_sub)        
        m0 = physics_get_m(this%phys,stat_info_sub)
        
        IF ( multiscale > 0 ) THEN
           
           m1 = physics_get_m1(this%phys,stat_info_sub)
           m2 = physics_get_m2(this%phys,stat_info_sub)
           
           !chi scale ratio=max_mass/min_mas
           
           SELECT CASE ( multiscale )
              
           CASE (1)
              ! linear change range for size of particles
              
              SELECT CASE ( multiscale_shape )
                 
              CASE (1)
                 ! monotonic linear increase of size
                 PRINT *, __FILE__, __LINE__, "multiscale shape not supported !"
                 stat_info = -1
                 GOTO 9999
                 
              CASE (-1)
                 ! monotonic linear decrease of size
                 PRINT *, __FILE__, __LINE__, "multiscale shape not supported !"
                 stat_info = -1
                 GOTO 9999
                 
              CASE (2)
                 ! square hat size distribution
                 ! this means symmetry, therefore, 
                 ! height is half of the box divided by chi_level
                 
                 height = (max_phys(2)-min_phys(2))/2.0_MK/chi_level
                 m_increment = (m2-m1)/(chi_level-1)
                 
                 DO i=1, this%num_part_real
                    
                    DO j=0, chi_level-1
                       
                       IF ((this%x(2,i) >= min_phys(2)  + j * height .AND. &
                            this%x(2,i) < min_phys(2)  + (j+1) * height ) .OR. & 
                            (this%x(2,i) < max_phys(2) - j * height .AND. &
                            this%x(2,i) >= max_phys(2) - (j+1) * height) ) THEN
                          ! middle half of the domain
                       this%m(i) = m1 + m_increment*j
                       
                       END IF
                       
                    END DO
                    
                 END DO
             
                 ! linear Hat size distribution.
                 ! and assume the middle has maximum size particle
                 !height = (max_phys(2)-min_phys(2))/2.0_MK 
                 ! slope of the mass change
                 !psi  = (m2-m1)/height 
                 
                 !DO i=1, this%num_part_real
                    
                 !   IF (this%x(2,i) < (min_phys(2) + max_phys(2))/2.0_MK) THEN
                       ! first half of the domain
                 !      this%m(i) = m1 + psi*(this%x(2,i) - min_phys(2))
                 !   ELSE
                       ! second half of the domain
                 !      this%m(i) = m1 + psi*(max_phys(2) - this%x(2,i))
                 !   END IF
                    
                 !END DO
                 
              CASE (-2)
                 ! inverse square hat size distribution
                 ! this means symmetry, therefore, 
                 ! height is half of the box divided by chi_level
                 
                 
                 height = (max_phys(2)-min_phys(2))/2.0_MK/chi_level
                 m_increment = (m2-m1)/(chi_level-1)
                 
                 DO i=1, this%num_part_real
                    
                    DO j=0, chi_level-1
                       
                       IF ((this%x(2,i) >= min_phys(2)  + j * height .AND. &
                            this%x(2,i) < min_phys(2)  + (j+1) * height) .OR. & 
                            (this%x(2,i) < max_phys(2) - j * height .AND. &
                            this%x(2,i) >= max_phys(2) - (j+1) * height)) THEN
                          ! middle half of the domain
                       this%m(i) = m2 - m_increment*j
                       
                       END IF
                       
                    END DO
                    
                 END DO
                 
                 ! inverse of linear hat for size distribution
                 ! and assume the middle has minimum size particle
                 ! height = (max_phys(2)-min_phys(2))/2.0_MK 
                 ! slope of the mass change
                 !psi  = (m2-m1)/height 
                 
                 !DO i=1, this%num_part_real
                    
                 !  IF (this%x(2,i) < (min_phys(2) + max_phys(2))/2.0_MK) THEN
                 ! first half of the domain
                 !     this%m(i) = m2 - psi*(this%x(2,i) - min_phys(2))
                 !ELSE
                       ! second half of the domain
                 !     this%m(i) = m2 - psi*(max_phys(2) - this%x(2,i))
                 !  END IF
                    
                 !END DO
                 
              END SELECT ! multiscale shape
              
           CASE (2)
              
              ! Parabolic functions of size distribution.
              SELECT CASE ( multiscale_shape )
                 
              CASE (1)
                 ! monotonic linear increase of size
                 PRINT *, __FILE__, __LINE__, "multiscale shape not supported !"
                 stat_info = -1
                 GOTO 9999
                 
              CASE (-1)
                 ! monotonic linear decrease of size
                 PRINT *, __FILE__, __LINE__, "multiscale shape not supported !"
                 stat_info = -1
                 GOTO 9999
                 
              CASE (2)
                
                 ! linear Hat size distribution.
                 ! and assume the middle has maximum size particle
                 !height = (max_phys(2)-min_phys(2))/2.0_MK 
                 ! slope of the mass change
                 !psi  = (m2-m1)/height**2
                 
                 !DO i=1, this%num_part_real
                 !   IF (this%x(2,i) < (min_phys(2) + max_phys(2))/2.0_MK) THEN
                       ! first half of the domain
                 !      this%m(i) = m1 + psi*(this%x(2,i) - min_phys(2))**2
                 !   ELSE
                       ! second half of the domain
                 !      this%m(i) = m1 + psi*(max_phys(2) - this%x(2,i))**2
                 !   END IF
                    
                 !END DO
                 
              CASE (-2)

                 ! inverse of linear hat for size distribution
                 ! and assume the middle has minimum size particle
                 !height = (max_phys(2)-min_phys(2))/2.0_MK 
                 ! slope of the mass change
                 !psi  = (m2-m1)/height**2
                 
                 !DO i=1, this%num_part_real
                    
                 !   IF (this%x(2,i) < (min_phys(2) + max_phys(2))/2.0_MK) THEN
                       ! first half of the domain
                 !      this%m(i) = m2 - psi*(this%x(2,i) - min_phys(2))**2
                 !   ELSE
                       ! second half of the domain
                 !      this%m(i) = m2 - psi*(max_phys(2) - this%x(2,i))**2
                 !   END IF
                    
                 !END DO
              
                 
              END SELECT ! multiscale shape

           CASE DEFAULT
              PRINT *, __FILE__, __LINE__, "multiscale type not supported!"
              stat_info = -1
              GOTO 9999
              
           END SELECT ! end multiscale
           
        ELSE
           
           this%m(1:this%num_part_real) = m0
           
        ENDIF ! multiscale
        

#if 0   
        
        !------------------------------------------
        ! Alternative way !
        !
        ! Calculate each particle's mass according
        ! to their volumes, i.e. mass=v*rho.
        !------------------------------------------
        
           
        !------------------------------------------
        ! Save the old rhs_density_type, and
        ! set it to 2, i.e. number density
        ! calculation, in order to calculate
        ! particles' mass according to each
        ! volume (1.0/num_density).
        !------------------------------------------
        
        symmetry = &
             control_get_symmetry(this%ctrl,stat_info_sub)
    
        rhs_density_type = &
             control_get_rhs_density_type(this%ctrl,stat_info_sub)
        CALL control_set_rhs_density_type(this%ctrl,2,stat_info_sub)
        CALL rhs_set_rhs_density_type(this%rhs,2,stat_info_sub)
             
        !-----------------------------------------------
        ! For mass density summation 
        ! e.g Morris et al 1997,
        ! it needs other particles' mass, therefore
        ! we have to map ghost of mass.
        !
        ! For number density summation 
        ! e.g. Espanol et al 2003,
        ! it doesn't need other particles' mass, 
        ! therefore we can allocate num_part_real memory.
        !
        ! However, to keep consistency of the interface
        ! of rhs_density_ff(), we map ghost always.
        !-----------------------------------------------        
        
        CALL particles_map_ghost_get(this, &
             l_map_x  = .TRUE., l_map_m = .TRUE., &
             l_map_id = .TRUE.,&
             stat_info=stat_info_sub)
        
        
        !----------------------------------------------------
        ! Get all particles' positions (including ghosts),
      	! to build neighbor list.
      	!----------------------------------------------------
        
        CALL technique_build_list(this%tech,this%x,&
             this%num_part_all,symmetry,stat_info_sub)
        
        IF (stat_info_sub /= 0) THEN
           PRINT *,'particles_compute_mass : ',&
                'Building lists failed !'
           stat_info = -1          
           GOTO 9999           
        END IF
        
        CALL particles_compute_density(this,stat_info_sub)
        
        IF( stat_info_sub /=0 ) THEN           
           PRINT *, "particles_compute_mass : ", &
                "Computing number density has problem !"           
           stat_info = -1
           GOTO 9999           
        END IF
        
        IF (symmetry) THEN
           
           CALL particles_map_ghost_put(this, &
                l_map_x = .TRUE., l_map_rho=.TRUE., &
                stat_info=stat_info_sub)
           
           IF( stat_info_sub /=0 ) THEN
              PRINT *, "particles_compute_mass : ", &
                   "Receiving number density from ghosts has problem !"
              stat_info = -1
              GOTO 9999           
           END IF
           
        END IF
        
        !--------------------------------
        ! Although the density of ghosts
        ! are not needed, shall we
        ! update ghosts again?
        ! Cause for vgt, it needs, 
        ! otherwise crashed.(12.08.2009)
        !--------------------------------
        
        this%m(1:this%num_part_real) = &
             init_rho / this%rho(1:this%num_part_real)
        
        !------------------------------------------
        ! Restore the old rhs density formulation.
        !------------------------------------------
        
        CALL control_set_rhs_density_type(this%ctrl,&
             rhs_density_type,stat_info_sub)
        CALL rhs_set_rhs_density_type(this%rhs,&
             rhs_density_type,stat_info_sub)
        
#endif
        
        
9999    CONTINUE
        
        
        IF(ASSOCIATED(min_phys)) THEN
           DEALLOCATE(min_phys)
        END IF

        IF(ASSOCIATED(max_phys)) THEN
           DEALLOCATE(max_phys)
        END IF
        
        RETURN
        
      END SUBROUTINE particles_compute_mass
      
      
