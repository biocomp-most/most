module m_pointpointer
!******************************************

  use m_point
  use m_path
  
  implicit none
  
  
  type t_pointpointer
       integer, private :: dayNumber ! 1:7
       integer, private :: idx
  
  contains  
        procedure,pass(self),public :: setPoint
        procedure,pass(self),public :: Point
        procedure,pass(self),public :: setnull
        procedure,pass(self),public :: isnull
        procedure,pass(self),public :: previousPtr
        procedure,pass(self),public :: nextPtr
        procedure,pass(self),public :: isSameLocation
        procedure,pass(self),public :: isSamePoint
        procedure,pass(self),public :: displayPointer
        procedure,pass(self),public :: pathLength
   !     procedure,pass(self),private :: calculate_defecations
  end type t_pointpointer     

 

 CONTAINS
 
!******************************************************************
  subroutine setPoint (self,dayNbr, idx)
!-----------------------------------------------------  
      class (t_pointpointer), intent(out) :: self
      integer, intent(in) :: dayNbr, idx
      
      self%dayNumber = dayNbr
      self%idx = idx
      
  end subroutine setPoint
!==============================================================         

type(t_point) FUNCTION  Point(self, paths )
      class (t_pointpointer), intent(in) :: self
      type(t_path),dimension(7), intent(in) :: paths
        
      Point = paths(self%dayNumber)%points(self%idx)
END FUNCTION Point
!==========================================
type(t_pointpointer) FUNCTION  previousPtr(self, paths )
      class (t_pointpointer), intent(in) :: self
      type(t_path),dimension(7), intent(in) :: paths
      
      integer :: dayIdx, pointIdx
      
      call previousPtr%setnull()
      
      dayIdx = self%dayNumber
      pointIdx = self%Idx - 1
      
      if (pointIdx < 0) then
       dayIdx = dayIdx -1
       if(dayIdx > 0) then
          pointIdx = paths(dayIdx)%maxPointIndex
       end if
      end if
      
      if (pointIdx .GE. 0) then
         call previousPtr%setPoint(dayIdx, pointIdx)
      end if

END FUNCTION previousPtr

!==========================================
type(t_pointpointer) FUNCTION  nextPtr(self, paths )
      class (t_pointpointer), intent(in) :: self
      type(t_path),dimension(7), intent(in) :: paths
      
      integer :: dayIdx, pointIdx
      
      call nextPtr%setnull()
      
      dayIdx = self%dayNumber
      pointIdx = self%Idx + 1
      
      if (pointIdx > paths(dayIdx)%maxPointIndex) then
       dayIdx = dayIdx + 1
       pointIdx = 0
      end if
      
      if (dayIdx .LE. size(paths)) then
         call nextPtr%setPoint(dayIdx, pointIdx)
      end if

END FUNCTION nextPtr
!==========================================
REAL FUNCTION  pathLength(self, otherPtr, paths )
      class (t_pointpointer), intent(in) :: self
      type(t_pointpointer), intent(in) :: otherPtr
      type(t_path),dimension(7), intent(in) :: paths
      
      type(t_pointpointer) :: currentPtr
      type(t_point) :: thePoint
      
      pathLength = 0.0
      currentPtr = self%nextPtr(paths)
 !     if (self%isSameLocation(currentPtr,paths)) then
  !       return ! No travel, length = 0
  !    end if
loopPtr:  do while ( .NOT. currentPtr%isNull() )
              thePoint = currentPtr%Point(paths)
              pathLength = pathLength + thePoint%step_length()
        
              if(currentPtr%dayNumber .EQ. otherPtr%dayNumber .AND. currentPtr%idx .EQ. otherPtr%idx  ) then
                exit loopPtr
              end if 
             currentPtr = currentPtr%nextPtr(paths)
          end do loopPtr
      

END FUNCTION pathLength
!******************************************************************

!==========================================
LOGICAL FUNCTION  isSameLocation(self, otherPtr, paths )
      class (t_pointpointer), intent(in) :: self
      type(t_pointpointer), intent(in) :: otherPtr
       type(t_path),dimension(7), intent(in) :: paths
      
      type(t_point) :: P1, P2
      
      P1 = self%Point(paths)
      P2 = otherPtr%Point(paths)
                 
      isSameLocation = (P1%coord(X) .EQ. P2%coord(X) .AND. P1%coord(Y) .EQ. P2%coord(Y))

END FUNCTION isSameLocation
!==========================================
LOGICAL FUNCTION  isSamePoint(self, otherPtr )
      class (t_pointpointer), intent(in) :: self
      type(t_pointpointer), intent(in) :: otherPtr
     
                
      isSamePoint = (self%dayNumber .EQ. otherPtr%dayNumber .AND. self%idx .EQ. otherPtr%idx)

END FUNCTION isSamePoint
!==========================================
CHARACTER(16) FUNCTION  displayPointer(self, paths)
      class (t_pointpointer), intent(in) :: self
      type(t_path),dimension(7), intent(in) :: paths
      
      type(t_point) :: pt 
      
      if(.NOT. self%isNull()) then
        pt = self%Point(paths)
        displayPointer = pt%format_point_idx()
      else
        displayPointer = 'null'
      end if
      
 
END FUNCTION displayPointer
!******************************************************************

!******************************************************************
  subroutine setnull (self)
!-----------------------------------------------------  
      class (t_pointpointer), intent(out) :: self
      self%dayNumber = -1
      
  end subroutine setnull
  
!==============================================================         

LOGICAL FUNCTION  isnull(self)
      class (t_pointpointer), intent(in) :: self
        
      isnull = (self%dayNumber .LT. 1)
END FUNCTION isnull  
!*********************************************  
end module m_pointpointer
