!**************************************************
module m_parameters

!
! Reads values in parameter.dat and store them in common variables declared in mod_common.f90
!**************************************************
  use m_common
  use m_tools
  
  implicit none

  public
  

 
!! functions  
  public :: get_CharParam, get_LogicalParam, get_DoubleParam, &
     get_DoubleParamArray

   contains 
!===========================================
  subroutine read_parameters(filePath)
!===========================================
!   Read parameter values from Parameter.dat file
!
    implicit none
     character(*),intent(in) :: filePath
     integer :: err, nbr_records, pos_column
     character(100) str
     integer, parameter :: OpenUnit = 1

     integer :: i,j, nbrVars
     
     nbrVars = size(prm_d_stateVars) ! dans mod_common
     
    open(unit=OpenUnit,file=filePath,status='old', iostat=err)
      if(err /= 0) then
         str = "File " // trim (filePath) // " not found."
         call fatal_error(str)
         return
      end if
     
     prm_c_input_dir = get_CharParam(OpenUnit)     
     prm_c_output_dir = get_CharParam(OpenUnit)
     
     ! input files 
     do i=1,12
       prm_c_MCPFile(i) = get_CharParam(OpenUnit)
     end do
     prm_c_NoGoFile = get_CharParam(OpenUnit)
     prm_c_SleepingSitesFile = get_CharParam(OpenUnit)
     prm_c_FruitTreesFilePeriodFiles(1) = get_CharParam(OpenUnit)
     prm_c_FruitTreesFilePeriodFiles(2) = get_CharParam(OpenUnit)
     prm_c_FruitTreesFilePeriodFiles(3) = get_CharParam(OpenUnit)
     prm_c_faidefFile = get_CharParam(OpenUnit)
     prm_c_BedWakeupTimeFile  = get_CharParam(OpenUnit)
     
     !! Output files 
     DefecationPointsFile = get_CharParam(OpenUnit)
     SpittedPointsFile = get_CharParam(OpenUnit)
     GeneratedWeeksXYFile  = get_CharParam(OpenUnit)
     week_summary_File  = get_CharParam(OpenUnit)
     rejected_weeks_File  = get_CharParam(OpenUnit)
     rejected_weeks_summary_File  = get_CharParam(OpenUnit)
     
     do i = 1, 15, 3
       call get_DoubleParamArray(OpenUnit, prm_d_stateVars(i:i+2))
     end do
     do i = 16, 27, 6
       call get_DoubleParamArray(OpenUnit, prm_d_stateVars(i:i+5))
     end do

     prm_i_month = get_IntegerParam(OpenUnit)
     if(prm_i_month < 1 .OR. prm_i_month > 12) then
         str = "The execution month in parameter file must be between 1 and 12."
         call fatal_error(str)
         return
     end if
     prm_i_NumberOfweeks = get_IntegerParam(OpenUnit)
     prm_i_maxWeekRejected= get_IntegerParam(OpenUnit)
     prm_i_maxStepsTrials= get_IntegerParam(OpenUnit) 
     prm_i_StateAtStart= get_IntegerParam(OpenUnit)
     prm_i_NumberOfDaysPerWeek= get_IntegerParam(OpenUnit)
     prm_i_stepDuration= get_IntegerParam(OpenUnit)
     prm_i_NestRadius=get_IntegerParam(OpenUnit)
     prm_i_TreeRadius=get_IntegerParam(OpenUnit)
     prm_i_MaxNumberOfFeedingsADay=get_IntegerParam(OpenUnit)
     prm_i_feedingDuration=get_IntegerParam(OpenUnit)
     prm_i_MinutesBeforeEndOfDay=get_IntegerParam(OpenUnit)
     
     ! Gut tarnsit and cheek pouch retention
     prm_i_gutTransitMean=get_IntegerParam(OpenUnit)
     prm_i_gutTransitSD=get_IntegerParam(OpenUnit)
     prm_d_gutTransitSDlimit=get_DoubleParam(OpenUnit)
     prm_i_CheekRetentionMean=get_IntegerParam(OpenUnit)
     prm_i_CheekRetentionSD=get_IntegerParam(OpenUnit)
     prm_d_CheekRetentionSDlimit=get_DoubleParam(OpenUnit)
      
     !Call print routines or not
     DspParameters = get_LogicalParam(OpenUnit)
     DspFruits = get_LogicalParam(OpenUnit)
     DspStates = get_LogicalParam(OpenUnit)
     DspDaytimes = get_LogicalParam(OpenUnit) 
     DspSleepingSites = get_LogicalParam(OpenUnit)
     DspFaiDef = get_LogicalParam(OpenUnit)
     DspTestRoutines = get_LogicalParam(OpenUnit)
     prm_l_dspDefecationDetails = get_LogicalParam(OpenUnit)
     
  

     close(OpenUnit) 
 
  end subroutine read_parameters

!=====================================
integer function get_IntegerParam(OpenUnit)
    integer, intent(in) :: OpenUnit
    character(15) :: str, fm
    integer :: l
   
    str = get_CharParam(OpenUnit)
    l = len_trim(str)
    read(str(1:l),*) get_IntegerParam
    
end function get_IntegerParam  
!=====================================
function get_LogicalParam(OpenUnit)
    integer, intent(in) :: OpenUnit
    Logical :: get_LogicalParam
    character(10) :: str
    
    str = get_CharParam(OpenUnit)
    read(str(1:1),'(L1)') get_LogicalParam
    
end function get_LogicalParam    
!=====================================
function get_DoubleParam(OpenUnit)
    integer, intent(in) :: OpenUnit
    double precision :: get_DoubleParam
    character(200) :: str
    
    str = get_CharParam(OpenUnit)
    read(str,'(G)') get_DoubleParam
    
end function get_DoubleParam  
!=====================================
subroutine get_IntegerParamArray(OpenUnit, arr)
    integer, intent(in) :: OpenUnit
    integer, dimension(:), intent(out) :: arr
    character(200) :: str
    integer :: i, pf, pl

                
    str = get_NextRecord(OpenUnit)

    pf = 1

    do i = 1, size(arr)
        pl = pf+1 
        do while (pl < len(str) .and. str(pl:pl) > ' ')
             pl = pl + 1
        end do
        read(str(pf:pl-1),'(I)') arr(i)
        pf = pl+1
        do while (pf < len(str) .and. ichar(str(pf:pf)) <= 32) 
          pf = pf + 1
        end do
    end do
    
  
end subroutine get_IntegerParamArray   
!============================= 
subroutine get_DoubleParamArray(OpenUnit, arr)
    integer, intent(in) :: OpenUnit
    double precision, dimension(:), intent(out) :: arr
    character(200) :: str
    integer :: i, pf, pl
   
                
    str = get_NextRecord(OpenUnit)
    pf = 1

    do i = 1, size(arr)
        pl = pf+1 
        ! do while (pl < len(str) .and. str(pl:pl) > ' ')
        do while (str(pl:pl) > ' ')
             pl = pl + 1
        end do
     !   write(*,'(2(I3,1X),A)') pf,pl-1, str
        read(str(pf:pl-1),'(G)') arr(i)
        pf = pl+1
        do while (pf < len(str) .and. ichar(str(pf:pf)) <= 32) 
          pf = pf + 1
        end do
    end do
    
  
end subroutine get_DoubleParamArray  
!=============================  

!=====================================
function get_NextRecord(OpenUnit)
   integer, intent(in) :: OpenUnit
    character(200) :: get_NextRecord
    character(200) :: str
    integer :: err, pos_column
    integer :: pf
while1: &
      do while (err == 0) 
         read (unit=1,fmt='(A)',iostat=err) str !! data line

        if(err /= 0) then
           str = "Parameters file not complete or missing columns ':' in lines"
           call fatal_error(str)
           return
        end if
        str = trim(str)
        pos_column = index(str,':',BACK=.TRUE.)
        if (pos_column > 0) then; exit; end if ! On traite le record
      end do while1  
      pf = pos_column + 1

     do while (str(pf:pf) <= ' ') 
           pf = pf + 1
     end do
     get_NextRecord = str(pf:)

end function get_NextRecord
!=====================================
function get_CharParam(OpenUnit)
!=====================================
    integer, intent(in) :: OpenUnit
    character(80) :: get_CharParam
    character(200) :: str
    integer :: pl
     
    str =  get_NextRecord(OpenUnit)    
    pl = 1 
    do while (pl < len(str) .and. str(pl:pl) > ' ')
         pl = pl + 1
         if (str(pl:pl) <= ' ') then
            pl = pl - 1
            exit
         end if 
    end do
    get_CharParam = str(1:pl)

end function get_CharParam    
!=====================================
subroutine Print_Parameters()
  integer :: i,j
  write(*,'(A)') '---- Parameters ------'
  write(*,'(3A)') 'prm_c_input_dir:',tab,prm_c_input_dir
  write(*,'(3A)') 'prm_c_output_dir:',tab,prm_c_output_dir
  write(*,*) ''  
  DO i=1,12 
    write(*,'(A,I2,5A)') 'prm_c_MCPFile',i,':',tab,tab,tab,prm_c_MCPFile(i)
  END DO
  write(*,'(4A)') 'prm_c_NoGoFile:',tab,tab,prm_c_NoGoFile
  write(*,'(3A)') 'prm_c_SleepingSitesFile:',tab,prm_c_SleepingSitesFile
  write(*,'(3A)') 'Bed-Wakeup-Time:',tab,prm_c_BedWakeupTimeFile
  write(*,'(A)') 'Step length parameters:'
  write(*,'(A)') '----------------------:'
  write (*,'(A,3E17.9)') 'mean         :',(prm_d_stateVars(i), i=1,3)
  write (*,'(A,3F17.9)') 'sd           :',(prm_d_stateVars(i), i=4,6)
  write (*,'(A,3E17.9)') 'zero-mass    :',(prm_d_stateVars(i), i=7,9)
  write(*,'(A)') 'Turning angle parameters:'
  write(*,'(A)') '------------------------:'
  write (*,'(A,3F17.8)') 'mean         :',(prm_d_stateVars(i), i=10,12)
  write (*,'(A,3F17.8)') 'concentration:',(prm_d_stateVars(i), i=13,15)
  write(*,'(A)') 'Regression coeff.:'
  write(*,'(A)') '-----------------:'
  write (*,'(A,6F17.8)') 'Intercept (beta0):',(prm_d_stateVars(i), i=16,21)
  write (*,'(A,6F17.8)') 'FAI_DEF (beta1)  :',(prm_d_stateVars(i), i=22,27)
  write(*,'(A)') '-----------------'
  write(*,'(A,I2,A,F7.3)') 'Execution month : ',prm_i_month, ", FAI_DEF :", FAI_DEF
  write(*,'(A,I)') 'Number of Weeks to be generated :', prm_i_NumberOfWeeks
  write(*,'(A,I)') 'Maximum of Weeks rejected       :', prm_i_maxWeekRejected
  write(*,'(A,I)') 'Maximum of step trials per Week :', prm_i_maxStepsTrials
  write(*,'(A,I)') 'State at start                   :', prm_i_StateAtStart
  write(*,'(A,I)') 'Number of days par Week         :', prm_i_NumberOfDaysPerWeek
  write(*,'(A,I)') 'step duration (minutes)          :', prm_i_stepDuration
  write(*,'(A,I)') 'max distance from the nest       :', prm_i_NestRadius
  write(*,'(A,I)') 'max tree distance for feeding    :', prm_i_TreeRadius
  write(*,'(A,I)') 'max number of feedings a day     :',prm_i_MaxNumberOfFeedingsADay
  write(*,'(A,I)') 'feeding duration at tree (min.)  :',prm_i_feedingDuration
  write(*,'(A,I)') 'minutes before end of day to go to nest:',prm_i_MinutesBeforeEndOfDay
  write(*,'(A)') '-----Gut transit and cheek pouch retention ------------'
  write(*,'(A,I)') 'Gut transit mean (min.)                :', prm_i_gutTransitMean
  write(*,'(A,I)') 'Gut transit standard deviation (min.)  :', prm_i_gutTransitSD
  write(*,'(A,F7.3)') 'Gut transit SD multiplier for limit :',  prm_d_gutTransitSDlimit

  write(*,'(A,I)') 'Cheek pouch retention mean (min.)      :', prm_i_CheekRetentionMean
  write(*,'(A,I)') 'Cheek pouch retention SD (min.)        :', prm_i_CheekRetentionSD
    write(*,'(A,F7.3)') 'Cheek pouch retention SD multiplier for limit :',  prm_d_CheekRetentionSDlimit
  !----------------


    write(*,'(3A)') 'DefecationPointsFile:',tab,DefecationPointsFile
    write(*,'(3A)') 'SpittedPointsFile:',tab,SpittedPointsFile
    write(*,'(3A)') 'GeneratedWeeksXYFile:',tab,GeneratedWeeksXYFile
    write(*,'(4A)') 'Week_summary:',tab,tab,Week_summary_File
    write(*,'(3A)') 'rejected_Weeks:',tab,rejected_Weeks_File
    write(*,'(3A)') 'rejected_Weeks_summary:',tab,rejected_Weeks_summary_File
 

  write(*,*) ''

  write(*,'(A)') '----------------------'
  
end subroutine Print_Parameters

  
end module m_parameters


