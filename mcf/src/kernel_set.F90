      SUBROUTINE kernel_set_cut_off(this,d_cut_off,stat_info)

        TYPE(Kernel), INTENT(IN)        :: this
        REAL(MK), INTENT(IN)            :: d_cut_off
        INTEGER, INTENT(OUT)            :: stat_info

        stat_info = 0

        this%cut_off = d_cut_off

        RETURN

      END SUBROUTINE  kernel_set_cut_off
