      SUBROUTINE kernel_init(this,d_num_dim, d_kernel_type,&
           d_cut_off,stat_info)
        !----------------------------------------------------
        ! Subroutine  :  kernel_init
        !----------------------------------------------------
        !
        ! Purpose     : Construtor of Class Kernel.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 03.03.2009, original version.
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
        
        TYPE(Kernel), INTENT(OUT)       :: this
        INTEGER, INTENT(IN)             :: d_num_dim
        INTEGER, INTENT(IN)             :: d_kernel_type
        REAL(MK), INTENT(IN)            :: d_cut_off
        INTEGER, INTENT(OUT)            :: stat_info
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        
        this%num_dim       = d_num_dim
        this%kernel_type   = d_kernel_type
        this%cut_off       = d_cut_off

        SELECT CASE(d_kernel_type)
           
           !-------------------------------------------------
           ! Lucy Kernel
           !-------------------------------------------------
           
        CASE (2)
           
           this%h = d_cut_off
           
           SELECT CASE(d_num_dim)
              
           CASE (1)
              
              this%coef = 5.0_MK / (4.0_MK * this%h)
              
           CASE (2)
              
              this%coef = 5.0_MK/(mcf_pi*this%h**2)
              
           CASE (3)
              
              this%coef = 105.0_MK/(16.0_MK*mcf_pi*this%h**3)
              
           END SELECT ! num_dim
           
           !-------------------------------------------------
           ! Quintic Spline.
           !-------------------------------------------------
           
        CASE (5)
           
           this%h = d_cut_off / 3.0_MK
           
           SELECT CASE (d_num_dim)
              
           CASE (1)
              
              this%coef = 1.0_MK/120.0_MK/this%h
              
           CASE (2)
              
              this%coef = 7.0_MK/(478.0_MK*mcf_pi*this%h**2)
              
           CASE (3)
              
              this%coef = 1.0_MK/(120.0_MK*mcf_pi*this%h**3)
              
           END SELECT ! num_dim
           
        CASE DEFAULT
           
           PRINT *, __FILE__, __LINE__, &
                this%kernel_type, &
                "No such kernel type!"
           stat_info = -1
           GOTO 9999
           
        END SELECT ! kernel_type
        
        this%coef_grad = -this%coef/this%h

9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE kernel_init
      
      
      SUBROUTINE kernel_display_parameters(this,stat_info)
        
        TYPE(Kernel), INTENT(IN)              :: this
        INTEGER, INTENT(OUT)                  :: stat_info
        
        
        stat_info = 0
        
        PRINT *, '------------------Start------------------'
        PRINT *,'      Kernel Parameters '
        PRINT *, '-----------------------------------------'
        
        
        PRINT *, "num_dim          : ", this%num_dim
        
        SELECT CASE (this%kernel_type)
           
        CASE (2)
           
           PRINT *, "kernel_type      : ", &
                "Lucy kernel "
        
        CASE (5)
           
           PRINT *, "kernel_type      : ", &
                "Quintic Spline "
           
        CASE DEFAULT
           
           PRINT *, __FILE__, __LINE__, &
                "No such kernel type!"
           stat_info = -1
           GOTO 9999
           
        END SELECT
        
        PRINT *, "smoothing length : ", this%h
        PRINT *, "cut off          : ", this%cut_off
        
        
        PRINT *, '-------------------End-------------------'
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE kernel_display_parameters
      
      
