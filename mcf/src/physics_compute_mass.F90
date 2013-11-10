      SUBROUTINE physics_compute_mass(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : physics_compute_mass
        !----------------------------------------------------
        !
        ! Purpose     : Compute particle mass and colloid mass
        !               Depending on lattice type
        !      
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 Nov. 4, 2013, original version.
        !     
        !----------------------------------------------------
        ! Author       : Xin Bian
        ! Contact      : xin_bian@brown.edu
        !
        ! Prof. George Em Karniadakis',
        ! Crunch group,
        ! the division of applied mathematics,
        ! Brown University, US.
        !----------------------------------------------------

        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(Physics), INTENT(INOUT)            :: this
        INTEGER, INTENT(OUT)                    :: stat_info

        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: multiscale
        INTEGER                                 :: multiscale_shape
        REAL(MK)                                :: resolution_ratio1
        REAL(MK)                                :: resolution_ratio2
        INTEGER                                 :: num_dim
        
        !----------------------------------------------------
        ! Colloid related variables
        !----------------------------------------------------
        
        INTEGER, DIMENSION(:), POINTER          :: coll_shape
        REAL(MK), DIMENSION(:,:), POINTER       :: coll_radius
        INTEGER, DIMENSION(:), POINTER          :: coll_freq
        REAL(MK), DIMENSION(:), POINTER         :: coll_m
        REAL(MK), DIMENSION(:,:), POINTER       :: coll_mmi
        REAL(MK)                                :: coll_vol
        REAL(MK)                                :: coll_m_tot
        REAL(MK)                                :: init_rho
        
        INTEGER                                 :: i
        REAL(MK)                                :: n_p
        REAL(MK)                                :: d_theta
        REAL(MK)                                :: theta
        REAL(MK)                                :: r
        REAL(MK)                                :: a, b, c, v_cap
        REAL(MK)                                :: ami
   
       
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
      
        multiscale = &
             control_get_multiscale(this%ctrl,stat_info_sub)
        
        num_dim   = this%num_dim
        
        NULLIFY(coll_shape)
        NULLIFY(coll_radius)
        NULLIFY(coll_freq)        
        NULLIFY(coll_m)
        NULLIFY(coll_mmi)
        
        !----------------------------------------------------
        ! Mass of colloids are calculated according
        ! to theire volumens, i.e. mass=v*rho.
        !----------------------------------------------------
        
        init_rho  = this%rho
        coll_m_tot = 0.0_MK
        
        IF ( this%num_colloid > 0 ) THEN
           
           CALL colloid_get_shape(this%colloids,coll_shape,stat_info_sub)
           CALL colloid_get_radius(this%colloids,coll_radius,stat_info_sub)
           CALL colloid_get_freq(this%colloids,coll_freq,stat_info_sub)
           CALL colloid_get_m(this%colloids,coll_m,stat_info_sub)
           CALL colloid_get_mmi(this%colloids,coll_mmi,stat_info_sub)

           coll_vol = 0.0_MK
           
           DO i = 1, this%num_colloid
              
              IF ( num_dim == 2 ) THEN
                 
                 SELECT CASE(coll_shape(i))
                    
                 CASE( mcf_colloid_shape_cylinder)
                    
                    !----------------------------------------
                    ! 2D cylinder area.
                    !----------------------------------------
                    
                    coll_vol = mcf_pi * coll_radius(1,i)**2.0_MK
                    coll_m(i) = init_rho * coll_vol
                    coll_mmi(3,i) = 0.5_MK*coll_m(i)*coll_radius(1,i)**2
                    
                 CASE( mcf_colloid_shape_disk)
                    
                    !----------------------------------------
                    ! 2D disk area.
                    !----------------------------------------
                    
                    coll_vol = mcf_pi * coll_radius(1,i)**2.0_MK
                    coll_m(i) = init_rho * coll_vol
                    coll_mmi(3,i) = 0.5_MK*coll_m(i)*coll_radius(1,i)**2
             
                    
                 CASE (mcf_colloid_shape_ellipse)
                    
                    !----------------------------------------
                    ! 2D ellipse area.
                    !----------------------------------------
                    
                    coll_vol = mcf_pi*coll_radius(1,i)*coll_radius(2,i)
                    coll_m(i)= init_rho*coll_vol
                    coll_mmi(3,i) = 0.25_MK*coll_m(i)* &
                         ( coll_radius(1,i)**2+coll_radius(2,i)**2 )
                    
                 CASE (mcf_colloid_shape_dicolloid)
                    
                    PRINT *, "physics_compute_mass: ", &
                         "Dicolloid in 2D not implemented."
                    stat_info = -1
                    GOTO 9999
                    
                 CASE (mcf_colloid_shape_star)
                    
                    !----------------------------------------
                    ! 2D star area.
                    !----------------------------------------
                    
                    !coll_vol = mcf_pi * coll_radius(1,i)**2
                    !coll_m(i)= init_rho * coll_vol
                    
                    n_p     = 10000.0_MK
                    d_theta = 2.0_MK*mcf_pi/n_p
                    theta   = d_theta / 2.0_MK
                    
                    coll_vol = 0.0_MK
                    ami      = 0.0_MK
                    
                    DO WHILE ( theta <= 2.0_MK*mcf_pi )
                       
                       r = colloid_polar_star_r(&
                            coll_radius(1,i),coll_radius(2,i), &
                            REAL(coll_freq(i),MK),theta,0.0_MK)
                       
                       coll_vol = coll_vol + r**2 * d_theta / 2.0_MK
                       ami      = ami + r**4 * d_theta / 4.0_MK
                       
                       theta = theta + d_theta
                       
                    END DO
                    
                    coll_m(i)   = coll_vol * init_rho
                    coll_mmi(3,i) = ami * init_rho              
                    
                 CASE  DEFAULT
                    
                    PRINT *, "physics_compute_mass: ", &
                         "No such shape in 2D !"
                    stat_info = -1
                    GOTO 9999
                
                 END SELECT ! shape(i)
                 
                 !-------------------------------------------
                 ! For symmetry boundaries, we reduce the
                 ! mass and moment inertia by factor num_sym.                
                 !-------------------------------------------
                 
                 !IF ( num_sym > 0 ) THEN
                    
                  !  coll_m(i)   = coll_m(i) / REAL(num_sym,MK)
                  !  coll_mmi(3,i) = coll_mmi(3,i) / REAL(num_sym,MK)
                    
                 !END IF
                 
                 
              ELSE IF ( num_dim ==3 ) THEN
                 
                 SELECT CASE(coll_shape(i))
                    
                    !----------------------------------------
                    ! momentum of inertia is not checked!
                    !----------------------------------------
                    
                 CASE(mcf_colloid_shape_cylinder)
                     
                    coll_vol = mcf_pi * &
                         coll_radius(1,i)**2.0_MK * &
                         coll_radius(2,i)
                    coll_m(i) = init_rho * coll_vol
                    coll_mmi(3,i) = 0.5_MK*coll_m(i)*coll_radius(1,i)**2
  
                 CASE(mcf_colloid_shape_sphere)
                    
                    !----------------------------------------
                    ! 3D Sphere volume.
                    !----------------------------------------
                    
                    coll_vol = &
                         4.0_MK* mcf_pi * &
                         coll_radius(1,i)**3.0_MK / 3.0_MK
                    
                    coll_m(i)       = init_rho*coll_vol
                    coll_mmi(1:3,i) = 0.4_MK*coll_m(i)*coll_radius(1,i)**2
                    
                 CASE(mcf_colloid_shape_ellipsoid)
                    
                    !----------------------------------------
                    ! 3D ellipsoid volume.               
                    !----------------------------------------
                    
                    coll_vol = 4.0_MK * mcf_pi * &
                         coll_radius(1,i)*coll_radius(2,i)*&
                         coll_radius(3,i)/3.0_MK
                    
                    coll_m(i) = init_rho * coll_vol
                    coll_mmi(1,i) = 0.2_MK*coll_m(i)* &
                         (coll_radius(2,i)**2+coll_radius(3,i)**2)
                    coll_mmi(2,i) = 0.2_MK*coll_m(i)* &
                         (coll_radius(1,i)**2+coll_radius(3,i)**2)
                    coll_mmi(3,i) = 0.2_MK*coll_m(i)* &
                         (coll_radius(1,i)**2+coll_radius(2,i)**2)
                   
                 CASE(mcf_colloid_shape_dicolloid)
                    !----------------------------------------
                    ! 3D dicolloid, check wikipedia for
                    ! spherical cap to get volume v_cap
                    !----------------------------------------
                    
                    a = coll_radius(1,i)
                    b = coll_radius(2,i)
                    c = SQRT(a**2 - b**2)
                    v_cap = mcf_pi * (2.0_MK * a + b) * &
                         (a - b)**2 / 3.0_MK
                    coll_vol = &
                         8.0_MK * mcf_pi * &
                         a**3.0_MK / 3.0_MK - &
                         2.0_MK * v_cap
                    
                    coll_m(i) = init_rho * coll_vol
                    
                    !----------------------------------------
                    ! check report for details
                    !----------------------------------------
                    
                    coll_mmi(1,i) = &
                         16.0_MK * init_rho * mcf_pi * a**5 / 15.0_MK - &
                         init_rho * mcf_pi * &
                         (a**4*c-2.0_MK*a**2*c**3/3.0_MK+c**5/5.0_MK)
                    
                    coll_mmi(2,i) = init_rho * mcf_pi * &
                         (b**5 + 10.0_MK*a**2*b**3+40.0_MK*a**3*b**2+&
                         45.0_MK*a**4*b+16.0_MK*a**5)/30.0_MK
                    
                    coll_mmi(3,i) = coll_mmi(2,i)
                    
                 CASE  DEFAULT
                    
                    PRINT *, "physics_compute_mass: ", &
                         "No such shape in 3D !"
                    stat_info = -1
                    GOTO 9999
                
                 END SELECT ! coll_shape
                 
                 !-------------------------------------------
                 ! For symmetry boundaries, we reduce the
                 ! mass and moment inertia by factor num_sym.
                 !-------------------------------------------
                 
                 !IF ( num_sym > 0 ) THEN
                 
                 !  coll_m(i)   = coll_m(i) / REAL(num_sym,MK)
                 ! coll_mmi(1:3,i) = coll_mmi(1:3,i) / REAL(num_sym,MK)
                    
                 !END IF
                 
              END IF
              
              coll_m_tot = coll_m_tot + coll_m(i)
              
           END DO ! i = 1 , num_colloid
           
           
           CALL colloid_set_m(this%colloids,coll_m,stat_info_sub)
           CALL colloid_set_mmi(this%colloids,coll_mmi,stat_info_sub)
           
        END IF ! num_colloid > 0

        !----------------------------------------------------
        ! Calculation of particle's mass
        ! Must be performed after colloids' mass is known
        !----------------------------------------------------
        
        IF ( num_dim == 2 ) THEN
           
           SELECT CASE ( this%lattice_type )
              
           CASE (mcf_lattice_type_square)
              !----------------------------------------------
              ! Here we consider 2D sqaure or 3D simple
              ! cubic lattice.
              !----------------------------------------------
              
              this%m = this%rho
              
              DO i = 1, num_dim
                 
                 this%m = this%m * this%dx(i)
                 
              END DO

           CASE (mcf_lattice_type_staggered)
              !-------------------------------------------------
              ! 2D staggered lattice, not verified!
              !-------------------------------------------------
              
              this%m = this%rho
              
              DO i = 1, num_dim
                 
                 this%m = this%m * this%dx(i) 
                 
              END DO
              
              this%m = this%m * 4.0_MK / 3.0_MK
              
           CASE (mcf_lattice_type_hexagonal)
              !-------------------------------------------------
              ! 2D hexagonal lattice, not verified!
              !-------------------------------------------------
              
              this%m = this%rho
              
              DO i = 1, num_dim
                 
                 this%m = this%m * this%dx(i)
                 
              END DO
              
              this%m = this%m * 1.5_MK
              
           CASE DEFAULT
              
              PRINT *, "physics_compute_mass: ",&
                   "lattice type not available !"
              stat_info = -1
              GOTO 9999
              
           END SELECT ! lattice_type
           
        ELSE IF ( num_dim == 3 ) THEN
           
           SELECT CASE ( this%lattice_type )
              
           CASE (mcf_lattice_type_cubic)
              !----------------------------------------------
              ! Here we consider 3D simple cubic lattice.
              !----------------------------------------------
              
              this%m = this%rho
              
              DO i = 1, num_dim
                 
                 this%m = this%m * this%dx(i)
                 
              END DO
           
           CASE DEFAULT
              
              PRINT *, "physics_compute_mass: ",&
                   "lattice type not available !"
              stat_info = -1
              GOTO 9999
              
           END SELECT ! lattice_type
           
        END IF
        
        !PRINT *, __FILE__, __LINE__, "mass", this%m
        
        ! consider different mass of multi-resolution
        
        IF ( multiscale > 0 ) THEN
           
           !chi scale ratio=max_mass/min_mass
           
           SELECT CASE ( multiscale )

           CASE (1)
              ! linear change range for size of particles
              
              this%dx1 = this%dx(1)*this%chi1
              this%dx2 = this%dx(1)*this%chi2
              
              !CASE (2)
              
              ! Parabolic functions of size distribution.
              !   this%m1 = 3.0_MK*this%m/(this%chi+2.0_MK);
              !   this%m2 = this%chi * this%m1
              
           CASE DEFAULT
              PRINT *, __FILE__, __LINE__, "multiscale type not supported!"
              stat_info = -1
              GOTO 9999
              
           END SELECT ! end multiscale type

           !------------------------------------------
           ! For square/cubic lattic only.
           ! Get mininum and maximum
           ! smoothing length and cut off.
           ! For now we consider dx=dy(=dz)
           !------------------------------------------
           
        END IF ! multiscale
        
        this%m1 = this%rho * (this%dx1)**num_dim
        this%m2 = this%rho * (this%dx2)**num_dim
        
        this%h1 = this%h * this%chi1
        this%h2 = this%h * this%chi2
        
        this%cut_off1 = this%cut_off * this%chi1
        this%cut_off2 = this%cut_off * this%chi2
        
        
9999    CONTINUE
        
        IF(ASSOCIATED(coll_shape)) THEN
           DEALLOCATE(coll_shape)
        END IF
        
        IF(ASSOCIATED(coll_radius)) THEN
           DEALLOCATE(coll_radius)
        END IF
        
        IF(ASSOCIATED(coll_freq)) THEN
           DEALLOCATE(coll_freq)
        END IF
        
        IF(ASSOCIATED(coll_m)) THEN
           DEALLOCATE(coll_m)
        END IF

        IF(ASSOCIATED(coll_mmi)) THEN
           DEALLOCATE(coll_mmi)
        END IF
      
        RETURN
        
      END SUBROUTINE physics_compute_mass
      
      
