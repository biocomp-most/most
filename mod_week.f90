!**************************************************
 module m_week
!**************************************************

  use m_defecation
  use m_pointpointer
  
  implicit none
  
  
  type t_week
       type(t_path),dimension(7), private :: paths
  
  contains  
        procedure,pass(self),public :: init => init_week
        procedure,pass(self),public :: getPoint
   !     procedure,pass(self),private :: calculate_defecations
  end type t_week     
 
   type(t_defecation),dimension(:),allocatable, private :: defecations
   type(t_defecation),dimension(:),allocatable, private :: spitted
   
 CONTAINS
 
 
!******************************************************************
  subroutine init_week (self,weekIndex)
!-----------------------------------------------------  
      class (t_week), intent(out) :: self
      integer, intent(in) :: weekIndex
     
      integer :: grp, i, error_counter,j, loop_counter, nestIndex, dayCounter
      character(15) :: str
      character(150) :: outfile
      character(2) :: pluriel

      character(15),dimension(4) :: strA
      character(15),dimension(4) :: strB
      integer :: sumTrials, nbrSteps, OuterLoopCounter, MaxLoops
      character(200) :: errorMessage
      logical :: ErrorFound
      integer(kind = kCoord),dimension(2) :: startNestCoord

 
     
            
      error_counter = 0
  
    
      call get_NewNest(startNestCoord,nestIndex)
      
      do dayCounter=1, size(self%paths)
       if (dayCounter .GT. 1) then
        nestIndex = self%paths(dayCounter-1)%eveningNestIndex  !! The wake-up nest equals bed nest of previous day
       end if

        call self%paths(dayCounter)%Create(weekIndex,dayCounter,nestIndex)
              
      end do

      call calculate_defecations(self)
      
      do dayCounter=1, size(self%paths)
      call self%paths(dayCounter)%deallocate_memory()
      end do
  end subroutine init_week  
  
 
 !******************************************************************
  type(t_point) FUNCTION getPoint (self,dayNbr,Idx)
!----------------------------------------------------- 
     class (t_week), intent(in) :: self 
     integer, intent(in) :: dayNbr, idx
     
     getPoint = self%paths(dayNbr)%points(Idx)
     
  end FUNCTION getPoint  

!******************************************************************
  subroutine calculate_defecations (week)
!-----------------------------------------------------  
      type (t_week), intent(in) :: week
      type (t_point) :: pt
      integer :: i,dayIndex, pointIndex, ierr
      integer, parameter ::  defecation_file = 5, spitted_file = 6
      integer :: nbr_defecations, nbr_spitted, idx_defecation, idx_spitted
      ! type(t_pointpointer) :: testPtr1, testPtr2
      ! real :: test1,test2
      
      ! count and allocate defecations points
      ! count and allocate spitted points
      nbr_defecations = 0
      nbr_spitted = 0
      
      do dayIndex=1, size(week%paths)
      if( .not. week%paths(dayIndex)%ErrorFound ) then
        do pointIndex = 0, week%paths(dayIndex)%maxPointIndex
        ! write (*,'(2(A,I4))') "dayindex:",dayindex," pointIndex:", pointIndex
          pt = week%paths(dayIndex)%points(pointIndex)
          if(pt%swallow) then
            nbr_defecations = nbr_defecations + 1
          end if
          if(pt%spitted) then
             nbr_spitted = nbr_spitted + 1
          end if
        end do
      end if
     end do 
     if (nbr_defecations > 0) then
          allocate(defecations(nbr_defecations), stat=ierr)
          if (ierr > 0) then
             call fatal_error('Insufficient memory to be allocated for defecation points.')
          end if 
     end if
     if (nbr_spitted > 0) then
          allocate(spitted(nbr_spitted), stat=ierr)
          if (ierr > 0) then
             call fatal_error('Insufficient memory to be allocated for spitted points.')
          end if 
     end if
     
     idx_defecation = 0
     idx_spitted = 0
     
     ! tables filling
      do dayIndex=1, size(week%paths)
      if( .not. week%paths(dayIndex)%ErrorFound ) then
        do pointIndex = 0, week%paths(dayIndex)%maxPointIndex
        ! write (*,'(2(A,I4))') "dayindex:",dayindex," pointIndex:", pointIndex
          pt = week%paths(dayIndex)%points(pointIndex)
          if(pt%swallow) then
            idx_defecation = idx_defecation + 1
            call defecations(idx_defecation)%init_defecation(pt)
            defecations(idx_defecation)%transit_duration = get_randomized_transit_time()
            call calculate_defecation_point (defecations(idx_defecation),week)
            write(defecation_file,'(A)') defecations(idx_defecation)%display_defecation_point()
          end if
          if(pt%spitted) then
             idx_spitted = idx_spitted + 1
             call spitted(idx_spitted)%init_defecation(pt)
             spitted(idx_spitted)%transit_duration = get_randomized_cheek_retention_time()
             call calculate_defecation_point (spitted(idx_spitted),week)
             write(spitted_file,'(A)') spitted(idx_spitted)%display_defecation_point()
          end if

        end do
      end if
     end do 

     
    if (nbr_spitted > 0) then  
      deallocate (spitted)
    end if
    if (nbr_defecations > 0) then
      deallocate (defecations)
    end if
   
    
    
  end subroutine calculate_defecations  

!******************************************************************
!******************************************************************
 subroutine calculate_defecation_point (pt,week)
   type(t_defecation), intent(out) :: pt
   type (t_week), intent(in) :: week
   
   type(t_pointpointer), dimension(2) :: ptrs
   type(t_pointpointer) :: ptr_tree
   type(t_point) :: P1, P2
   integer :: i,dayIndex, pointIndex, ierr
   integer :: timeOfDefecation
   integer :: time_P1_pt, duration_between_P1_P2
   real :: dist_P1_P2, traveledDist, dist_P1_ejection
   real :: speed_P1_P2
   type(t_edge) :: edge_P1_P2
   character(32),dimension(3) :: fmtInt
   
   do i=1,2 
    call ptrs(i)%setNull()
   end do
  timeOfDefecation = pt%timeOfWeek + pt%transit_duration
  
  ! searches the upper limit of the segment where the defecation point is located
  loopday:   do dayIndex=pt%dayNumber, size(week%paths)
  loopidx: do pointIndex = 0, week%paths(dayIndex)%maxPointIndex
            if (timeOfDefecation .LE. week%paths(dayIndex)%points(pointIndex)%time_of_week()) then
               call ptrs(2)%setPoint(dayIndex,pointIndex)
               exit loopidx
            end if
        end do loopidx
        if (.NOT. ptrs(2)%isNull() ) then
           exit loopday
        end if
   end do loopday
   
   if (ptrs(2)%isNull() ) then
       pt%note = "The ejection point is beyond last step of week"
       RETURN
   end if
   
   ptrs(1) = ptrs(2)%previousPtr(week%paths)
   if(ptrs(1)%isNull()) then
       pt%note = "NO start point found. Bug. Case not possible. Call the developer."
       RETURN
   end if
   
       
    P1 = ptrs(1)%Point(week%paths)
    P2 = ptrs(2)%Point(week%paths)
    call ptr_tree%setPoint(pt%dayNumber, pt%point_idx)
    
    dist_P1_P2 = P1%distance_from_point(P2)
    duration_between_P1_P2 = P2%time_of_week() - P1%time_of_week()
   
    
    pt%P1_coord = P1%coord
    pt%P2_coord = P2%coord
    pt%P1_idx = P1%format_point_idx()
    pt%P2_idx = P2%format_point_idx()
    
    if(ptr_tree%isSamePoint(ptrs(1))) then
      traveledDist = 0.0
    else 
      traveledDist = ptr_tree%pathLength(ptrs(1),week%paths)
    end if
        
    
    if(P1%nestIndex > 0 .AND. P2%nestIndex > 0) then
      write(pt%note,'(A,X,A)') "Ejection in nest", pt%P1_idx
      pt%defecation_coord = P1%coord
      pt%realdefecation_coord = REAL(pt%defecation_coord)
      
     else if (ptrs(1)%isSameLocation(ptrs(2),week%paths)) then 
       fmtInt(1) = "Long stay at ejection point :"
       write(fmtInt(2),*) duration_between_P1_P2
       fmtInt(3) = "minutes."
       write (pt%note,'(2(A,X),A)') (trim(adjustl(fmtInt(i))),i=1,3)        
     
       pt%defecation_coord = P1%coord
       pt%realdefecation_coord = REAL(pt%defecation_coord)
      
    else if(duration_between_P1_P2 > 0 ) then
      ! intermediate point calculation between P1 and P2
      speed_P1_P2 = dist_P1_P2 / REAL(duration_between_P1_P2)
      dist_P1_ejection = speed_P1_P2 * (timeOfDefecation - P1%time_of_week())
      edge_P1_P2%startP = P1%realCoord
      edge_P1_P2%endP   = P2%realCoord
      pt%realdefecation_coord = point_at_dist(edge_P1_P2, dist_P1_ejection)
      pt%defecation_coord = NINT(pt%realdefecation_coord)
      traveledDist = traveledDist + dist_P1_ejection
    
    
    else
      pt%defecation_coord = P1%coord
      pt%realdefecation_coord = REAL(pt%defecation_coord)

    
    end if
    
     pt%traveled_dist = traveledDist
     pt%shortest_dist= distance(pt%realdefecation_coord, pt%realTree_Coord)
  
      
  
 end subroutine calculate_defecation_point  
  !**************************************************
 end module m_week
!**************************************************  
  
