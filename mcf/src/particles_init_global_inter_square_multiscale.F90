      SUBROUTINE  particles_init_global_inter_square_multiscale(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_init_global_inter_square_multiscale
        !----------------------------------------------------
        !
        ! Purpose     : Create positions for particles in 
        !               total physical domain on square 
        !               lattice, considering multiscale.
        !
        ! Routines    :
        !
        ! Remarks     : Most often used initial positions
        !               for particles.
        !
        ! References  :
        !
        ! Revisions   : V0.1 Nov. 7,2013, original version.
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
    	! Modules :
    	!----------------------------------------------------
        
        USE ppm_module_find_duplicates
        
        !----------------------------------------------------
        ! Arguments :
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER,INTENT(OUT)	                :: stat_info
        
        !----------------------------------------------------
    	! Local variables start here :
        !
        ! num_dim    : number of dimension.
        ! min_phys_t : minimal coordinate of total domain,
        !              including solid wall if there is.
        ! max_phys_t : maximum coordinate of total domain,
        !              including solid wall if there is.
        ! num_part_tot : 
        !              number of estimated physical particles,
        !              if particles are evenly distributed.
        !              
        ! dx         : initial distance between two particles;
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        INTEGER                                 :: multiscale_shape
        INTEGER                                 :: num_dim
        REAL(MK), DIMENSION(:,:), POINTER       :: x_local      
        REAL(MK), DIMENSION(:), POINTER         :: min_phys
        REAL(MK), DIMENSION(:), POINTER         :: max_phys
        REAL(MK), DIMENSION(:), POINTER         :: min_phys_t
        REAL(MK), DIMENSION(:), POINTER         :: max_phys_t
        
        INTEGER                                 :: num_part_tot
        REAL(MK), DIMENSION(:), POINTER         :: dx

        REAL(MK), DIMENSION(2)                  :: sx
        REAL(MK)                                :: dx1,dx2
        REAL(MK)                                :: height
        
        !----------------------------------------------------
        ! Counters.
        !----------------------------------------------------
        
        INTEGER                                 :: num        
        
        !----------------------------------------------------
    	! Initialization of variables.
    	!----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(x_local)
        NULLIFY(min_phys)
        NULLIFY(max_phys)
        NULLIFY(min_phys_t)
        NULLIFY(max_phys_t)
        NULLIFY(dx)
        
        !----------------------------------------------------
        ! Get physics including boundary parameters.
        !----------------------------------------------------
        
        multiscale_shape = &
             physics_get_multiscale_shape(this%phys,stat_info_sub)
        num_dim = this%num_dim        
        CALL physics_get_min_phys(this%phys, &
             min_phys,stat_info_sub)
        CALL physics_get_max_phys(this%phys, &
             max_phys,stat_info_sub)
        CALL physics_get_min_phys_t(this%phys, &
             min_phys_t,stat_info_sub)
        CALL physics_get_max_phys_t(this%phys, &
             max_phys_t,stat_info_sub)
        
        num_part_tot = &
             physics_get_num_part_tot(this%phys,stat_info_sub)
        CALL physics_get_dx(this%phys,dx,stat_info_sub)
        dx1 = physics_get_dx1(this%phys,stat_info_sub)
        dx2 = physics_get_dx2(this%phys,stat_info_sub)
        !PRINT *, "dx1, dx2: ", dx1, dx2
        !----------------------------------------------------
    	! Allocate memory for particles.
        !
	! x   : position
	! v   : velocity
       	! id  : particle ID,  species ID
    	!----------------------------------------------------
        ALLOCATE(x_local(num_dim,3*num_part_tot))
        !ALLOCATE(this%v(num_dim,num_part_tot), &
        !     STAT=stat_info_sub)
        !ALLOCATE(this%id(this%num_id,num_part_tot), &
        !     STAT=stat_info_sub)
        
        IF( stat_info_sub /= 0 ) THEN
           PRINT *, &
                "particles_init_global_inter_square_multiscale : ", &
                "Allocating memory for variables has problem !"
           stat_info = -1
           GOTO 9999
        END IF
	
        
        !----------------------------------------------------
        ! Create positions of particles.
        ! Walls are not included!
        !----------------------------------------------------
        
        num = 0
        
        height = max_phys(2)-min_phys(2)
        
        SELECT CASE ( multiscale_shape )
           
        CASE (2)
           ! square hat
           ! smallest particle (first part)
           
           sx(2) = min_phys(2) + 0.5_MK * dx1

           DO WHILE( sx(2) < min_phys(2)+ height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx1
           
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx1
              
              END DO ! sx(1)
           
              sx(2) = sx(2) + dx1
           
           END DO  ! sx(2)

           ! largest particle
           sx(2) = min_phys(2) + height/4.0_MK + 0.5_MK * dx2

           DO WHILE( sx(2) < max_phys(2) - height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx2
           
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx2
              
              END DO ! sx(1)
           
              sx(2) = sx(2) + dx2
           
           END DO  ! sx(2)
           
           ! smallest particle (second part)
           
           sx(2) = max_phys(2) - 0.5_MK * dx1
           !sx(2) = min_phys(2) + 3.0_MK*height/4.0 + 0.5_MK * dx1

           DO WHILE( sx(2)> max_phys(2) - height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx1
           
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx1
              
              END DO ! sx(1)
           
              sx(2) = sx(2) - dx1
           
           END DO  ! sx(2)

        CASE (-2)
           ! inverse square hat
           ! largest particle (first part)
           
           sx(2) = min_phys(2) + 0.5_MK * dx2

           DO WHILE( sx(2) < min_phys(2)+ height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx2
           
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx2
              
              END DO ! sx(1)
           
              sx(2) = sx(2) + dx2
           
           END DO  ! sx(2)

           ! smallest particle
           sx(2) = min_phys(2) + height/4.0_MK + 0.5_MK * dx1

           DO WHILE( sx(2) < max_phys(2) - height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx1
           
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx1
              
              END DO ! sx(1)
           
              sx(2) = sx(2) + dx1
           
           END DO  ! sx(2)
           
           ! largest particle (second part)
           
           !sx(2) = min_phys(2) + 3.0_MK*height/4.0 + 0.5_MK * dx2
           sx(2) = max_phys(2) - 0.5_MK * dx2
           
           DO WHILE( sx(2) > max_phys(2) - height/4.0_MK )
           
              sx(1) = min_phys(1) + 0.5_MK * dx2
              
              DO WHILE( sx(1) < max_phys(1) )
              
                 num = num + 1
                 x_local(1:num_dim,num) = sx(1:num_dim)
              
                 sx(1) = sx(1) + dx2
              
              END DO ! sx(1)
           
              sx(2) = sx(2) - dx2
           
           END DO  ! sx(2)
           
        CASE DEFAULT
           
           PRINT *, __FILE__, __LINE__, &
                "not such multiscale shape!"
           stat_info = -1
           GOTO 9999
           
           
        END SELECT
        
        this%num_part_real = num
        ALLOCATE(this%x(num_dim,num), &
             STAT=stat_info_sub)
        !PRINT *, "height: ", height
        !PRINT *, "num: ", num
        this%x(1:num_dim,1:num) = x_local(1:num_dim,1:num)
         
        !----------------------------------------------------
        ! Return.
        !----------------------------------------------------
        
9999	CONTINUE	
        
        IF(ASSOCIATED(x_local)) THEN
           DEALLOCATE(x_local) 
        END IF

        IF(ASSOCIATED(min_phys)) THEN
           DEALLOCATE(min_phys) 
        END IF
        
        IF(ASSOCIATED(max_phys)) THEN
           DEALLOCATE(max_phys) 
        END IF

        IF(ASSOCIATED(min_phys_t)) THEN
           DEALLOCATE(min_phys_t) 
        END IF
        
        IF(ASSOCIATED(max_phys_t)) THEN
           DEALLOCATE(max_phys_t) 
        END IF
        
        IF(ASSOCIATED(dx)) THEN
           DEALLOCATE(dx) 
        END IF
        
        RETURN
        
      END SUBROUTINE particles_init_global_inter_square_multiscale
      
      
