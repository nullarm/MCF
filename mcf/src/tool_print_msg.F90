      SUBROUTINE tool_print_msg_b(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_b
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing Boolean message 
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        LOGICAL       	                :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,L)") TRIM(msg), val
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_b
      
      
      SUBROUTINE tool_print_msg_f(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_f
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing float message 
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        REAL(MK)       	                :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,F9.6)") TRIM(msg), val
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_f


      SUBROUTINE tool_print_msg_i(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_s
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing integer message 
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        INTEGER           	        :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,I8)") TRIM(msg), val

        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_i
      

      SUBROUTINE tool_print_msg_is(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_s
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing integer and
        !               string message for displaying purpose.
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        INTEGER           	        :: val1
        CHARACTER(LEN=*)       	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,I8,A)") TRIM(msg), val1, TRIM(val2)

        PRINT *, TRIM(msg)
        
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_is


      SUBROUTINE tool_print_msg_s(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_s
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing string message 
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        CHARACTER(LEN=*) 	        :: val
        INTEGER          	        :: stat_info
        
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: " // TRIM(val)
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_s
            
      
      SUBROUTINE tool_print_msg_si(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_si
      	!----------------------------------------------------
	!
      	! Purpose     : Subroutine of printing string and integer
        !               message for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
      	! Arguments     
      	!----------------------------------------------------

        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*) 	        :: name
        CHARACTER(LEN=*)       	        :: val1
        INTEGER           	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, name_suffix
        CHARACTER(LEN=MAX_CHAR)	        :: msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ":"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: " // TRIM(val1)
        
        WRITE(msg, "(A,I8)") TRIM(msg), val2
        
        PRINT *, TRIM(msg)
        
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_si

      
