!**************************************************
module m_daytime

! - stores an array of 12 months mean and SD for wake-up and bed time.
! - get_newStartEndDayTimes function provides times following normal distribution
!**************************************************
  use m_common
  use m_tools

  use cdf_normal_mod

  
  implicit none
  private 

  type t_daytime
     integer,private :: entryTime, entrySD, exitTime, exitSD ! minutes from hour 0h00 (midnight)
   contains
   
    procedure,pass(self),public :: init => init_daytime

  end type t_daytime

  type(t_daytime),dimension(12),save :: daytimes ! one per month

  public :: create_daytimes
  public :: display_daytime 
  private :: get_Daytime
  private :: InvNormaDist
  public :: test_normalDist
  public :: get_newStartEndDayTimes

  CONTAINS
     
!!
 !==============================================================   
 subroutine init_daytime (self, heures)
!--------------------------------------------------------------
    class (t_daytime), intent(out) :: self
    character(5), dimension(4), intent(in) :: heures

    self%entryTime = get_MinutesFromTime(heures(1))
    self%entrySD = get_MinutesFromTime(heures(2))
    self%exitTime = get_MinutesFromTime(heures(3))
    self%exitSD = get_MinutesFromTime(heures(4))

 end subroutine init_daytime 
 
!=================================================   
   subroutine create_daytimes()
!=================================================   
    use m_tools
    implicit none
    
      character(80) :: filePath
      
      integer :: err, i, mm
 	  character(160) :: str
      character(20), dimension(2) :: str_parts
      character(5), dimension(4) :: heures
      character(6) :: mois_annee
      logical :: trouve

      filePath = create_filepath(prm_c_input_dir, prm_c_BedWakeupTimeFile)
      !// Count number of records
      open(unit=1,file=filePath,status='old', iostat=err)
      if(err /= 0) then
         str = "File " // trim (filePath) // " not found."
         call fatal_error(str)
         return
      end if
      read (unit=1,fmt=*,iostat=err) str !! header line
while1:   do while (err == 0) 
            read (unit=1,fmt='(A6,4(X,A5))',iostat=err) mois_annee, heures !! data line
            if (err /= 0) then; exit; end if
  
            mm = stringMonth_to_integer(mois_annee(1:3), prm_c_BedWakeupTimeFile)
            call daytimes(mm)%init(heures)
          end do while1
      close(1)    
       
               
     end subroutine create_daytimes   

!==========================================================    
  integer function InvNormaDist(mean, sd)
!-------------------------------------------------------------------          

! Voir : https://keisan.casio.com/exec/system/1180573189

    integer, intent(in) :: mean, sd
    double precision :: cum, ccum, retour
       
    integer :: sts
    logical :: check_input = .false.
    
    call random_number(cum) 
    ccum = 1.0d0-cum
      
    retour = inv_normal(cum,ccum,dble(mean),dble(sd),sts,check_input)
    if(sts /= 0) then
      write(*,*) "***************************************************"
      write(*,'(A,I3)') "Fatal error in inv_normal function, status = ", sts
      write(*,'(A,F11.8,2(A,I4))') "cum=",cum,", mean=",mean, ", sd=" , sd
      write(*,*) "***************************************************"
      stop 
    end if
    
    InvNormaDist = NINT(retour)
    
  end function InvNormaDist          
!================================================= 
      subroutine display_daytime()
!=================================================      
      implicit none
      integer :: i
     integer, dimension(2) :: startend
      write(*,*) '****************************************'
      write(*,*) 'Bed wakeup exit times'
      write(*,*) '-----------------------'
      write(*,'(A)') 'Month   Entry    SD     Exit     SD'
      write (*,'(I2,5X,8A)') (i, &
        tab,get_TimeFromMinutes(daytimes(i)%entryTime), &
        tab,get_TimeFromMinutes(daytimes(i)%entrySD), &
        tab,get_TimeFromMinutes(daytimes(i)%exitTime), &
        tab,get_TimeFromMinutes(daytimes(i)%exitSD), &
           i=1,size(daytimes))
      write(*,'(A)')
      write (*,'(I2,A,I4,A,I4,A,I4,A,I4)') (i, &
        tab,daytimes(i)%entryTime, &
        tab,daytimes(i)%entrySD, &
        tab,daytimes(i)%exitTime, &
        tab,daytimes(i)%exitSD, &
           i=1,size(daytimes))
      write(*,'(A)')
      write(*,'(A)') 'Some start-end daytime'
      do i=1,10
         startend = get_newStartEndDayTimes()
         write(*,'(I3,4A)') i, tab, get_TimeFromMinutes(startend(1)), &
             tab, get_TimeFromMinutes(startend(2))
      
      end do
      
      
     end subroutine display_daytime    

!================================================= 
      subroutine test_normalDist()
!=================================================      
      implicit none
      integer :: i,res
 
      character(100) :: outfile
      
      integer, dimension(289:469) :: times ! 3 heures de 60' de 17 à 20h
      type(t_daytime) :: daytime
  
      
      daytime = get_Daytime(prm_i_month)
      times = 0
      
      write(*,'(A)') 'Month   Entry    SD     Exit     SD'
       write (*,'(I2,A,I4,A,I4,A,I4,A,I4)')  prm_i_month, &
        tab,daytime%entryTime, &
        tab,daytime%entrySD, &
        tab,daytime%exitTime, &
        tab,daytime%exitSD
           
      write(*,'(A)')
      
      
      do i = 1, 1000000
        res = InvNormaDist(daytime%exitTime, daytime%exitSD)
        if (res .GE. 289 .AND. res .LE. 469 ) then
           times(res) = times(res) + 1
        end if 
      end do
      
      ! output
      outfile = create_filepath(prm_c_output_dir, 'testNormalDist.csv')
      open(1,file=outfile,status='replace',action='write')
      write (1,'(2(A,:,A1))') "Minutes",tab,"Count"
      write (1,'(I4,A,I10)') (i, tab, times(i), i=289,469)


      close(1)
      
     end subroutine test_normalDist 
!===================================================================================
function  get_Daytime(le_shortMonthName)
!--------------------------------------------------------------------------------
        integer,  intent(in) :: le_shortMonthName   
        type(t_daytime) :: get_Daytime
        character(160) :: str
        if(le_shortMonthName .LT. 1 .OR. le_shortMonthName .GT. size(daytimes) ) then
               str = "mod_faidef, function get_faidef(month), invalid parameter, month must be in range 1..12" 
               call fatal_error(str)
              return
        end if
        get_Daytime = daytimes(le_shortMonthName)
 end function get_Daytime

!===================================================================================
function  get_newStartEndDayTimes() RESULT (w)
!!! Number of minutes from 00:00
!--------------------------------------------------------------------------------
     integer,  dimension(2) :: w
     type(t_daytime) :: daytime
     
     daytime = get_Daytime(prm_i_month)
     w(1) = InvNormaDist(daytime%exitTime, daytime%exitSD)  ! start of the day
     w(2) = InvNormaDist(daytime%entryTime, daytime%entrySD)  ! end of the day
      
  end function get_newStartEndDayTimes 
  
!**************************************************
end module m_daytime
!**************************************************  



