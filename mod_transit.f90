module m_transit

  use m_parameters
  use m_tools
  use cdf_normal_mod 
  
  implicit none
  
  private 
  
   public :: get_randomized_transit_time
   public :: get_randomized_cheek_retention_time
   public :: test_transit_time

      
   contains

     
!==========================================================    
  double precision function InvNormalDistCum(cum, mean, std_deviation)
!-------------------------------------------------------------------          
    double precision, intent(in) :: cum, mean, std_deviation
    double precision :: ccum
    
    integer :: sts
    logical :: check_input = .false.
    
    ccum = 1.0d0-cum
    ! inv_normal is in cdf_normal_mod 
    InvNormalDistCum = inv_normal(cum,ccum,mean,std_deviation,sts,check_input)
    if(sts /= 0) then
      write(*,*) "***************************************************"
      write(*,'(A,I3)') "Fatal error in inv_normal function, status = ", sts
      write(*,'(3(A,F11.8))') "cum=",cum,", mean=",mean, ", std_deviation=" , std_deviation
      write(*,*) "***************************************************"
      stop 
    end if

    
  end function InvNormalDistCum  

!==================================================
   FUNCTION get_randomized_time(mean,SD,multiplier)
!==================================================  
    integer,intent(in) :: mean,SD
    double precision, intent(in) :: multiplier
! https://oroboro.com/non-uniform-random-numbers/
    integer :: get_randomized_time
    
    double precision :: ranval,transit_mean,transit_SD_limit,transit_SD
    double precision x
    double precision minx, maxx
    
    transit_mean = DBLE(mean)
    transit_SD=DBLE(SD)
    transit_SD_limit = transit_SD*multiplier
    
    ! transit_mean, transit_std_deviation come from parameters.dat
    ! and located in m_common
    
    minx = transit_mean - transit_SD_limit
    if (minx .LT. 0.0D0) then
        minx = 0.0D0
    end if
    maxx = transit_mean + transit_SD_limit
 
    x = minx - 1.0D0
  !  write(*,'(4(F10.3,X))') transit_mean, transit_SD, minx, maxx
    do while (x .lt. minx .or. x .gt. maxx) 
       call random_number(ranval)
       x = InvNormalDistCum(ranval, transit_mean, transit_SD)
       !write(*,*) ranval, x
    enddo
   
    get_randomized_time = ABS(nint(x))

   END FUNCTION get_randomized_time 
 
!==================================================
   FUNCTION get_randomized_transit_time()
!==================================================  
! https://oroboro.com/non-uniform-random-numbers/
    implicit none 
    integer :: get_randomized_transit_time
    
    get_randomized_transit_time = get_randomized_time(prm_i_gutTransitMean, prm_i_gutTransitSD,prm_d_gutTransitSDlimit)

   END FUNCTION get_randomized_transit_time   
   
   !==================================================
   FUNCTION get_randomized_cheek_retention_time()
!==================================================  
! https://oroboro.com/non-uniform-random-numbers/
    implicit none 
    integer :: get_randomized_cheek_retention_time
    
    get_randomized_cheek_retention_time = get_randomized_time(prm_i_CheekRetentionMean,prm_i_CheekRetentionSD,prm_d_CheekRetentionSDlimit)

   END FUNCTION get_randomized_cheek_retention_time   
!====================================================
   SUBROUTINE test_transit_time()
   implicit none
   integer,dimension(1700:2100) :: minutes
   integer i,j


  minutes=0
  
  do i=1, 10000000
   j = get_randomized_transit_time()
  if (j.GE.1700 .AND. j.LE.2100) then
   minutes(j) = minutes(j) + 1
  end if
  enddo
  do i=1700,2100
      !if(minutes(i) .GT.1) then
        write(*,'(I9,2X,I10)') i, minutes(i)  
      !end if
  end do
   
   END SUBROUTINE test_transit_time
   
 end module m_transit