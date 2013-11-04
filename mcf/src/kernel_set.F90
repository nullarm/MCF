      SUBROUTINE kernel_set_cut_off(this,d_cut_off,stat_info)

        TYPE(Kernel), INTENT(INOUT)     :: this
        REAL(MK), INTENT(IN)            :: d_cut_off
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0

        this%cut_off = d_cut_off

        SELECT CASE(this%kernel_type)
           
           !-------------------------------------------------
           ! Quintic Spline.
           !-------------------------------------------------
           
        CASE (1)
           
           this%h = d_cut_off / 3.0_MK
           
           SELECT CASE (this%num_dim)
              
           CASE (1)
              
              this%coef = 120.0_MK/this%h
              
           CASE (2)
              
              this%coef = 7.0_MK/(478.0_MK*mcf_pi*this%h**2)
              
           CASE (3)
              
              this%coef = 3.0_MK/(359.0_MK*mcf_pi*this%h**3)
              
           END SELECT ! num_dim
           
           !-------------------------------------------------
           ! Lucy Kernel
           !-------------------------------------------------
           
        CASE (2)
           
           this%h = d_cut_off
           
           SELECT CASE(this%num_dim)
              
           CASE (1)
              
              this%coef = 5.0_MK / (4.0_MK * this%h)
              
           CASE (2)
              
              this%coef = 5.0_MK/(mcf_pi*this%h**2)
              
           CASE (3)
              
              this%coef = 105.0_MK/(16.0_MK*mcf_pi*this%h**3)
              
           END SELECT ! num_dim
           
        END SELECT ! kernel_type
        
        this%coef_grad = -this%coef/this%h

        RETURN

      END SUBROUTINE  kernel_set_cut_off
