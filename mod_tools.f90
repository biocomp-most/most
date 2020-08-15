!******************************************************
! tools routines
!******************************************************
module m_tools
  use m_common
    implicit none
  contains
!------------------------------------------------------
    function create_filepath(path, filename)
        character(*), intent(in) :: path, filename
        character(80) :: create_filepath
        create_filepath = trim(path) // trim(filename)
    end function create_filepath

!-------------------------------------------------------    
    subroutine fatal_error(msg)
     character(*) :: msg
     
     write (*,*) "************************************"
     write (*,*) "FATAL ERROR"
     write (*,*) msg
     write (*,*) "Program stopped"
     write (*,*) "************************************"
     stop
    end subroutine fatal_error

!==============================================
SUBROUTINE   split_string(str, str_parts, separator)
   character(*),intent(in) :: str
   character(*),dimension(:), intent(out) :: str_parts
   character(1),intent(in) :: separator 
   
   integer :: pf, pl, i,length
   length = len_trim(str)
   
   str_parts = ''
   pl = 0
   do i = 1, size(str_parts)
       pf = pl + 1 
       pl = pf
       do while (pl < length  .and. str(pl:pl) /=  separator)
         pl = pl + 1
         if (str(pl:pl) == separator) then
            str_parts(i) = str(pf:pl-1)
            exit 
         else if (pl == length) then
            str_parts(i) = str(pf:pl)
            exit
         end if 
       end do 
    end do
END SUBROUTINE split_string

!=====================================
integer function string_to_int(str)
    character(*),intent(in) :: str
    integer :: l

    l = len_trim(str)
    read(str(1:l),*) string_to_int
    
end function string_to_int  

!=====================================
real function string_to_real(str)
    character(*),intent(in) :: str
    read(str,'(G)') string_to_real
  
end function string_to_real  
!=====================================
double precision function string_to_double(str)
    character(*),intent(in) :: str
    read(str,'(G)') string_to_double
  
end function string_to_double 
!=====================================
! stringMonth_to_integer
!  exemple : 
!  stringMonth_to_integer("Mar 17", "FAI_DEF.csv")
!   returns 3
!===================================
integer function stringMonth_to_integer(str, filename)

    character(*),intent(in) :: str, filename
    character(3), dimension(12), parameter :: shortMonthName = &
      (/ "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec" /) 

    LOGICAL :: trouve
    INTEGER :: mm
    character(160) :: errorMessage
    
    trouve = .FALSE.
    stringMonth_to_integer = 0
    do mm=1,size(shortMonthName)
         if (shortMonthName(mm) .EQ. str(1:3) ) then
            trouve = .TRUE.
            stringMonth_to_integer = mm
            exit
         end if
    end do
    if(.NOT. trouve) then
         errorMessage = "In file " // trim (filename) // ": the month " // str // " doesn't exists."
         call fatal_error(errorMessage)
         return
    end if
    
end function stringMonth_to_integer

!===================================================================================
! get_TimeFromMinutes(totalminutes) : nombre de minutes --> hh:mm
!  exemple : 
!  get_TimeFromMinutes(122)
!   returns '02:02'  (deux heures et deux minutes)
!===================================
 character(5) function  get_TimeFromMinutes(totalminutes)
!--------------------------------------------------------------------------------
        integer , intent(in) :: totalminutes 
              
        write(get_TimeFromMinutes, '(I2.2,A,I2.2)') (totalminutes / 60),':',MODULO(totalminutes, 60)

end function get_TimeFromMinutes

!===================================================================================
! get_MinutesFromTime(strTime) : hh:mm --> nombre de minutes
!  exemple : 
!  get_MinutesFromTime('02:02')
!   returns 122  (deux heures et deux minutes)
!===================================
 integer function  get_MinutesFromTime(strTime)
!--------------------------------------------------------------------------------
       character(5) , intent(in) :: strTime 
       integer :: hh,mm
       
       read(strTime, '(I2.2,1X,I2.2)')  hh,mm
       get_MinutesFromTime = hh*60 + mm

end function get_MinutesFromTime

integer function Radian_to_Degrees( rad )
    real, intent(in) :: rad
     
     Radian_to_Degrees = NINT(180.0 * rad / real_PI)
end function Radian_to_Degrees

!=====================================================      
character(5) FUNCTION month_name()
!----------------------------------------------------- 
    month_name = prm_c_MCPFile(prm_i_month)(4:8)
    
END FUNCTION month_name
 !******************************************************************
 function file_name(strName) result(modifiedString)
 !----------------------------------------------------- 
    character(len=*), intent(in) :: strName
    character(len=:), allocatable :: modifiedString , filename
    character(2) :: prefixe

    
    if(prm_i_month .LT. 10) then
         write(prefixe,'(A,I1)') "0",prm_i_month
    else 
        write(prefixe,'(I2)') prm_i_month
    end if
    modifiedString = prefixe // '_' // month_name() // '_' // trim(strName)


 
 end function file_name
 
end module m_tools 