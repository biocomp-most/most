!**************************************************
module m_faidef

! - reads an array of 12 FAI_DEF values in FAI_DEF.csv file, one for each month. FAI_DEF is used in state calculations
! - stores the FAI_DEF value for the current month in FAI_DEF common variable
!
!**************************************************
  use m_common
  use m_tools

  
  implicit none

  private 
  
  
  real, dimension(12), save :: faidef

 
  public :: create_faidef
  public :: print_faidef
  private :: get_faidef

 
  
  CONTAINS
  
    
!!
 
!=================================================   
   subroutine create_faidef()
!=================================================   
    use m_tools
    implicit none
    
      character(80) :: filePath
      
      integer :: err, i, mm
 	  character(160) :: str
      character(20), dimension(2) :: str_parts

      logical :: trouve

      filePath = create_filepath(prm_c_input_dir, prm_c_faidefFile)
      !// Count number of records
      open(unit=1,file=filePath,status='old', iostat=err)
      if(err /= 0) then
         str = "File " // trim (filePath) // " not found."
         call fatal_error(str)
         return
      end if
      read (unit=1,fmt=*,iostat=err) str !! header line
while1:   do while (err == 0) 
            read (unit=1,fmt='(A)',iostat=err) str !! data line
            if (err /= 0) then; exit; end if
            call split_string(str, str_parts, ";")
            mm = stringMonth_to_integer(str_parts(1), prm_c_faidefFile)
            read(str_parts(2), '(G)') faidef(mm)
         end do while1
      close(1)    
       
      FAI_DEF = get_faidef(prm_i_month)  ! declared in mod_common
 
          
     end subroutine create_faidef    
!================================================= 
      subroutine print_faidef()
!=================================================      
      implicit none
      integer :: i

      write(*,*) '****************************************'
      write(*,*) 'FAI_DEF'
      write(*,*) '-----------'
      write(*,'(A)') 'shortMonthName   Value'
      write (*,'(I2,1X,F8.3)') (i, get_faidef(i),i=1,size(faidef))
      
     end subroutine print_faidef    

!===================================================================================
function  get_faidef(le_shortMonthName)
!--------------------------------------------------------------------------------
        integer,  intent(in) :: le_shortMonthName   
        real :: get_faidef
        character(160) :: str
        if(le_shortMonthName .LT. 1 .OR. le_shortMonthName .GT. size(faidef) ) then
               str = "mod_faidef, function get_faidef(month), invalid parameter, month must be in range 1..12" 
               call fatal_error(str)
              return
        end if
        get_faidef = faidef(le_shortMonthName)
 end function get_faidef
      
!**************************************************
end module m_faidef
!**************************************************  
  
   

