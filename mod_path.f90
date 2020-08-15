!**************************************************
 module m_path
 
 ! object t_path contains array of points for one day from wake-up to bed
 !**************************************************
  use m_common
  use m_step
  use m_angle
  use m_tools
  use m_point
  use m_geo
  use m_daytime
  
  implicit none
  
  
  type t_path
       integer :: weekIndex
       integer :: dayNumber
       type(t_point),dimension(:),allocatable, public :: points
       integer :: MorningTime
       integer :: EveningTime  
       integer :: maxPointIndex
       integer :: morningNestIndex
       integer :: eveningNestIndex
       integer :: NbrTrials
       logical :: ErrorFound
       character(200) :: errorMessage
       
   contains
   
    procedure,pass(self),public  :: Create
    procedure,pass(self),public :: deallocate_memory
    procedure,pass(self),public :: generate_points
    procedure,pass(self),public :: toString => display_path 

    
  end type t_path
  
 
  CONTAINS

!==============================================================   
  subroutine Create (self,weekIndex,dayNumber,startNestIndex)
      class (t_path), intent(out) :: self
      integer, intent(in)  :: startNestIndex
      integer, intent(in) :: weekIndex
      integer, intent(in) :: dayNumber
      
      integer,dimension(2) :: times
      integer :: NbrPoints, ierr, nextIndex

     !       write (*,'(A,X,I)') 'path create daynumber', dayNumber
      
      self%WeekIndex =weekIndex
      times = get_newStartEndDayTimes()
      self%maxPointIndex = 0
      self%dayNumber = dayNumber
      
      self%NbrTrials =0
      self%ErrorFound=.FALSE.
      self%errorMessage=''
      
      self%MorningTime = times(1)
      self%EveningTime = times(2)
      NbrPoints = ((times(2)-times(1)) / prm_i_stepDuration) + 10
      allocate(self%points(0:NbrPoints), stat=ierr)
      if (ierr > 0) then
         call fatal_error('Insufficient memory to be allocated. Decrease the number of points to be generated')
      end if 
      self%morningNestIndex = startNestIndex
      self%eveningNestIndex = 0
      ! first_point (self, firstState, dayNumber, startNestIndex, starttime_of_day)
      call self%points(0)%first_point(prm_i_StateAtStart,weekIndex,dayNumber,startNestIndex,self%MorningTime)
      
      call self%generate_points()
      
      self%eveningNestIndex = self%points(self%maxPointIndex)%nestIndex
      
  end subroutine Create


!=================================================================================== 
 subroutine generate_points(self)
      class (t_path) :: self
      integer ::  i, error_counter,j, loop_counter
      character(15) :: str
      character(150) :: outfile
      character(2) :: pluriel
   integer, parameter ::   generated_grps_file = 1, grp_summary_file = 2, &
           rejected_grps_file = 3, rejected_grp_summary_file = 4
      character(15),dimension(4) :: strA
      character(15),dimension(4) :: strB
      integer :: sumTrials, nbrSteps, OuterLoopCounter, MaxLoops
      logical :: searchNest


      ! generated_grps_file = 1
      ! grp_summary_file = 2
      ! rejected_grps_file = 3
      ! rejected_grp_summary_file = 4
      
      
      error_counter = 0
      loop_counter = 0

        do i = 1, size(self%points) - 1
          self%maxPointIndex = i
          call self%points(i)%next_point(self%points(i-1),self%EveningTime)
          if (self%points(i)%error_found) then
             error_counter = error_counter + 1
             self%ErrorFound = .true.
             self%errorMessage = self%points(i)%error_message

             exit  !! on ne calcule pas les derniers steps du jour
          elseif (self%points(i)%nestIndex .GT. 0) then
            ! On est au nid, la journée est finie, on sort
             exit  
          
          end if
        end do 


        if( .not. self%ErrorFound ) then
              ! !! write detail counters
              
               do j = 0,self%maxPointIndex
                  write(generated_grps_file,'(A)') self%points(j)%toString()
               enddo
               write(grp_summary_file,'(A)') self%toString()
               


        else
  
         !! write detail counters
              do j = 0,self%maxPointIndex
                  write(rejected_grps_file,'(3A)') self%points(j)%toString(),tab, trim(self%points(j)%error_message)
              enddo
              write(rejected_grp_summary_file,'(3A)') self%toString(),tab, trim(self%errorMessage)
  
         end if 
      
       
         
      
 end subroutine generate_points

!===================================================================================
character(256) function display_path(self)
!--------------------------------------------------------------------------------
      class (t_path), intent(in) :: self

      character(16),dimension(6) :: str
      integer :: i
      type(t_tree) :: atree
      type(t_nest) :: theNest
      
       write(str(1),*) self%weekIndex
       write(str(2),*) self%dayNumber
       write(str(3),*) self%maxPointIndex
       write(str(4),'(A)') get_TimeFromMinutes(self%MorningTime)
       write(str(5),'(A)') get_TimeFromMinutes(self%EveningTime)
       write(str(6),'(A)') get_TimeFromMinutes(self%points(self%maxPointIndex)%time_of_day)
      
      display_path = ''
  !   write(display_point,'(2A,I6,A,I7,22A)') trim(adjustl(str(1))),tab, self%coord(X), & 
   !         tab,self%coord(Y),(tab,trim(adjustl(str(i))),i=2,size(str))
      write(display_path,'(11A)') trim(adjustl(str(1))), (tab,trim(adjustl(str(i))),i=2,6)
     
 end function display_path 
 
!===================================================================================
character(256) function display_path_header()
!--------------------------------------------------------------------------------
   write(display_path_header,'(11A)') 'run#',tab,'day',tab,'points nbr',tab,'morning',tab,'evening forseen',tab,'evening actual'
   
 end function display_path_header 


!===================================================================================
 
!================================================= 
 subroutine deallocate_memory(self)
      class (t_path) :: self
      deallocate (self%points)
      
 end subroutine deallocate_memory
 
!**************************************************
 end module m_path
!**************************************************  