!**************************************************
 module m_point
!**************************************************

  use m_common
  use m_state
  use m_trees
  use m_nest
  use m_geo
  use m_tools
  use m_parameters
  

  implicit none
  integer, parameter :: X = 1
  integer, parameter :: Y = 2
  
  
  type t_point
    integer :: weekIndex 
    integer :: dayNumber ! 1:7
    integer :: nrState
    integer :: idx
    integer(kind = kCoord),dimension(2) :: coord

    real,dimension(2) :: realCoord,realdefecation_coord,realSpitted_coord
    integer ::  defecation_duration, spitted_duration
    real, private :: rho
    double precision, private :: randomAngle
    real, private :: theta
    integer, public :: time_of_day
    integer,private :: stepDuration
    integer :: treeIndex
    logical :: swallow
    logical :: spitted
    integer :: nestIndex
    integer :: nbrTrials  ! number of trials
    logical :: error_found
    character(200) :: error_message
    logical :: end_of_day
      
    
  contains
    procedure,pass(self),private  :: init
    procedure,pass(self),private  :: terminate
    procedure,pass(self),public  :: next_point
    procedure,pass(self),public  :: search_Nest
    procedure,pass(self),public  :: first_point
    procedure,pass(self),private :: search_Tree
    
    procedure,pass(self),public  :: toString => display_point
    
    procedure,pass(self),private  :: slopesSurroundingSecondPoint ! two slopes
    procedure,pass(self),private  :: PassThroughTree
    procedure,pass(self),public  :: time_of_week
    procedure,pass(self),public  :: distance_from_point
    procedure,pass(self),public  :: format_point_idx
    procedure,pass(self),public  :: step_length

  end type t_point
  
  
  CONTAINS

!==============================================================   
  subroutine init (self)
      class (t_point), intent(out) :: self
      self%weekindex = 0
      self%dayNumber = 0
      self%idx = 0
      self%nrState = 0
      self%coord = 0
      self%realCoord = 0.0
      self%defecation_duration = 0
      self%spitted_duration = 0
      self%rho = 0.0d0
      self%theta = 0.0
      self%randomAngle = 0.0d0
      self%treeIndex = 0
      self%swallow  = .FALSE.
      self%spitted  = .FALSE.
      self%nbrTrials = 0
      self%error_found = .false.
      self%error_message = ''
      self%nestIndex =0
      self%time_of_day=0
      self%stepDuration=0
      self%end_of_day = .FALSE.
      
  end subroutine init
!==============================================================   
    subroutine terminate (self)
      class (t_point), intent(out) :: self
        self%realCoord = real(self%coord)
  end subroutine terminate  
!--------------------------------------------------------------
  
!==============================================================   
   subroutine first_point (self, firstState, weekIndex, dayNumber, startNestIndex, starttime_of_day)
!--------------------------------------------------------------
      class (t_point), intent(out) :: self
      integer, intent(in)  :: firstState
      integer,intent(in) :: weekIndex
      integer,intent(in) :: dayNumber
      integer, intent(in)  :: startNestIndex
      integer,intent(in) :: starttime_of_day
      
      type(t_nest) :: startNest
      
      call self%init()
      startNest = get_nest(startNestIndex) 
      self%WeekIndex = weekIndex
      self%dayNumber = dayNumber
      self%nrState = firstState
      self%coord = startNest%coord
      self%realCoord = real(self%coord)
      self%nestIndex = startNestIndex
      self%time_of_day = starttime_of_day

     
 end subroutine first_point 

!==============================================================  
function get_step_duration (P1 ,P2 ,stoppoint) 
  !! step duration calculation when stop et tyree or nest
      real,dimension(2),intent(in) :: P1,P2,stoppoint
      integer :: get_step_duration
      
      real :: P1_P2, P1_STOP
      
      P1_P2 = distance(P1,P2)
      P1_STOP = distance(P1,stoppoint)
    !  Write(*,'(2(A,X,F10.4))') 'P1_P2',P1_P2,' P1_STOP',P1_STOP
      if (NINT(P1_P2) .LE. prm_i_TreeRadius) then
              get_step_duration = prm_i_stepDuration
      else 
              get_step_duration = ABS(NINT(real(prm_i_stepDuration)*P1_STOP/P1_P2))
      end if



end function get_step_duration
 
!==============================================================  
REAL function distance_from_point (self ,P2) 
      class (t_point), intent(in) :: self
      type(t_point),intent(in) :: P2
                  
      distance_from_point = distance(self%realCoord,P2%realCoord)
      
end function distance_from_point
 
 !==============================================================   
  LOGICAL FUNCTION search_Nest (self, previous, EveningTime )
!--------------------------------------------------------------
    class (t_point), intent(out) :: self
    type(t_point), intent(in) :: previous
    integer, intent(in) :: EveningTime
    
    logical :: endOfDay


  
    integer :: counter,i
    character(200) :: msg
    double precision :: angle_toNewPoint !! angle du point vers le point suivant
    real :: dist_between_points
    logical :: NestFound
    type(t_nest) :: theNest
    
    call self%init()
    self%WeekIndex = previous%weekIndex
    self%idx = previous%idx + 1
    self%dayNumber = previous%dayNumber
    self%nbrTrials = 0 
    self%nrState = state(previous%nrState)%get_NewState()
    counter = 0
    self%error_found = .false.
    self%error_message = ''
    ! DistanceAroundTheNest is a parameter in mod_common
    endOfDay = ((previous%time_of_day + prm_i_stepDuration) .GE. (EveningTime - prm_i_stepDuration))
           
    
 loopcounter:   do while (.true.)
        counter = counter + 1
       ! write(*,'(A,I)') 'loopcounter dans search_Nest : ' , counter
        self%nbrTrials = counter
        self%rho = state(self%nrState)%step%get_RandomStep()
        self%randomAngle =  state(self%nrState)%angle%get_RandomAngle()
        self%theta = previous%theta + real(self%randomAngle)
        if( self%theta < 0.0 ) THEN 
            self%theta = self%theta + angle360
        else if (self%theta >= angle360 ) THEN
            self%theta = self%theta - angle360 
        end if
        self%coord(X) = previous%coord(X) + (self%rho * cos(self%theta))
        self%coord(Y) = previous%coord(Y) + (self%rho * sin(self%theta))
        self%realCoord = real(self%coord)

        if(is_PointInKernel(self%coord)) then
           ! Searching nest if end of day
               NestFound = .FALSE. 
             if (counter .GE. prm_i_maxStepsTrials) then
              !! nest not found then we take the closest nest
              self%nestIndex = get_nearest_nest_index(self%realCoord)
              theNest = get_nest(self%nestIndex)
              NestFound = .TRUE.
         !     write (*,'(A,I)') "get_nearest_nest ", self%dayNumber
             else
     loopnests:    do i=1,size_of_nests()
                     theNest = get_nest(i)
                     NestFound = self%PassThroughTree(previous, theNest%coord, prm_i_NestRadius )
                     if (NestFound) then
                         !! On s'arrête au nid
                         self%nestIndex=i
                         exit loopnests
                     end if 
                   end do loopnests  
              end if                   
               if(NestFound) then
                       !        write(*,'(A)') 'NestFound'
                                        !! self%realCoord is theoretical point calculated following randomStep and randomAngle
                 !!   will be changed to nest coordinates
                   
                     self%stepDuration= prm_i_stepDuration ! get_step_duration(previous%realCoord,self%realCoord, real(theNest%coord))
                  !   write (*, '(A,I)') "Nestfounf, on ajoute " , self%stepDuration
                     self%time_of_day=previous%time_of_day+self%stepDuration
                     self%coord=theNest%coord
                     self%realCoord = real(self%coord)
                     self%end_of_day = endOfDay
                 exit loopcounter !! On prend le point qui est un nid
         
               else if (.NOT. endOfDay) then
                 EXIT loopcounter
 
                 
               else if (endOfDay .AND. counter .GT. prm_i_maxStepsTrials) then
                    write(*,'(A)') 'endOfDay .AND. counter .GT. prm_i_maxStepsTrials'
                   write(msg,'(A,I6,A,I6,A,i7,A)') "No next nest found after ", prm_i_maxStepsTrials, ' trials for point ', &
                      previous%coord(X), ',', previous%coord(Y) , '.'
                    ! write(*,*) msg
                   self%error_found = .true.     
                   self%error_message = msg 
                   exit loopcounter !! No Nest found
               end if
        end if
          
                     
     end do loopcounter
     
     if (NestFound) then
        call self%terminate()
     end if
     search_Nest = NestFound
 end function search_Nest
 
 
 !==============================================================   
  LOGICAL FUNCTION search_tree (self, previous )
!--------------------------------------------------------------
    class (t_point), intent(out) :: self
    type(t_point), intent(in) :: previous
    
    type(t_tree) :: theTree
    integer :: i
    search_tree = .FALSE.
    
    looptrees:  do i=1,size_of_trees()
           theTree = get_tree(i)
           search_tree = self%PassThroughTree(previous, theTree%coord, prm_i_TreeRadius )
             if (search_tree) then
                 !! we stop at the tree
                 !! self%realCoord is theoretical point calculated following randomStep and randomAngle
                 !!   will be changed to tree coordinates
                 self%stepDuration= get_step_duration(previous%realCoord,self%realCoord, real(theTree%coord))
                 self%stepDuration=self%stepDuration + prm_i_feedingDuration  
                 self%time_of_day=previous%time_of_day + self%stepDuration
                 self%treeIndex =i
                 self%coord=theTree%coord
                 self%realCoord = real(self%coord)
                 self%swallow = theTree%swallow
                 self%spitted = theTree%spitted
                 exit looptrees
             end if 
           end do looptrees 
    
       if (search_tree) then
        call self%terminate()
     end if

END FUNCTION search_tree
!==============================================================   
  SUBROUTINE next_point (self, previous, EveningTime )
!--------------------------------------------------------------
    class (t_point), intent(out) :: self
    type(t_point), intent(in) :: previous
    integer, intent(in) :: EveningTime
    
    logical :: SearchNest, endOfDay


  
    integer :: counter,i
    character(200) :: msg
    double precision :: angle_toNewPoint !! angle du point vers le point suivant
    real :: dist_between_points
    
    searchNest = ((previous%time_of_day + prm_i_stepDuration) .GE. (EveningTime - prm_i_MinutesBeforeEndOfDay))
    endOfDay = ((previous%time_of_day + prm_i_stepDuration) .GE. (EveningTime - prm_i_stepDuration))
    
    if (searchNest) then 
        if( self%search_nest(previous, EveningTime)) then  
           ! nest found
           ! we exit with that point
         !  write(*,'(A,I,1X,L)') "RETURN dans next point ", self%dayNumber, endOfDay
          
           RETURN
        end if
        if (self%error_found) then
          return
        end if
        if (endOfDay) then
           write(msg,'(A,I6,A,I6,A,i7,A)') "No nest found", &
             previous%coord(X), ',', previous%coord(Y) , '.'
           ! write(*,*) msg
           self%error_found = .true.     
           self%error_message = msg 
        end if
    end if



    call self%init()
    self%WeekIndex = previous%weekIndex
    self%idx = previous%idx + 1
    self%dayNumber = previous%dayNumber
    self%nbrTrials = 0 
    self%nrState = state(previous%nrState)%get_NewState()
    counter = 0
    self%error_found = .false.
    self%error_message = ''

    
 loopcounter:   do while (.true.)
        counter = counter + 1
        self%nbrTrials = counter
        self%rho = state(self%nrState)%step%get_RandomStep()
        self%randomAngle =  state(self%nrState)%angle%get_RandomAngle()
        self%theta = previous%theta + real(self%randomAngle)
        if( self%theta < 0.0 ) THEN 
            self%theta = self%theta + angle360
        else if (self%theta >= angle360 ) THEN
            self%theta = self%theta - angle360 
        end if
        self%coord(X) = previous%coord(X) + (self%rho * cos(self%theta))
        self%coord(Y) = previous%coord(Y) + (self%rho * sin(self%theta))
        self%realCoord = real(self%coord)
        
        if(is_PointInKernel(self%coord)) then
           
          ! do we pass through a tree ?
           if (.NOT. self%search_tree(previous )) then
                self%stepDuration = prm_i_stepDuration
                self%time_of_day=previous%time_of_day+self%stepDuration
           end if
           exit loopcounter !! On prend le point
           
        else if (counter >= prm_i_maxStepsTrials) then
           write(msg,'(A,I6,A,I6,A,i7,A)') "No next step found after ", prm_i_maxStepsTrials, ' trials for point ', &
             previous%coord(X), ',', previous%coord(Y) , '.'
           ! write(*,*) msg
           self%error_found = .true.     
           self%error_message = msg 
           exit loopcounter
        end if
     end do loopcounter
     
     
     if (.not. self%error_found) then
        if (self%TreeIndex .LE. 0 .AND. self%NestIndex .LE. 0 ) THEN

        END IF
         call self%terminate()
     end if 

   
 end subroutine next_point 
 !=================================================================================== 
character(16) function format_point_idx(self)
     class (t_point), intent(in) :: self
   
   character(16),dimension(5) :: strIdx
   integer :: i
      
      write(strIdx(1),*) self%dayNumber
      strIdx(2) = "-"
      write(strIdx(3),*) self%idx
      strIdx(4:5) = ''
      if (self%treeIndex .gt. 0 .OR. self%nestIndex .gt. 0) then 
         write(strIdx(4),*) "-"
         if (self%treeIndex .gt. 0) THEN 
           strIdx(5) = "T"
         else 
           strIdx(5) = "N"
         end if
       end if
       
    write(format_point_idx,'(5A)') (trim(adjustl(strIdx(i))),i=1,5)
    
end function format_point_idx
!=================================================================================== 
real function step_length(self)
     class (t_point), intent(in) :: self
   
       step_length = self%rho
    
end function step_length

!===================================================================================
character(256) function display_point(self)
!--------------------------------------------------------------------------------
      class (t_point), intent(in) :: self

      character(16),dimension(17) :: str
      integer :: i
      type(t_tree) :: atree
      type(t_nest) :: theNest
      
     
      
       write(str(1),*) self%weekIndex
       write(str(2),*) self%dayNumber
  !     write(str(3),*) self%idx
       str(3) = self%format_point_idx()
       write(str(4),*) self%nrState
       write(str(5),*) self%coord(X)
       write(str(6),*) self%coord(Y)
       write(str(7),'(F13.8)') self%rho
       write(str(8),'(F13.8)') self%randomAngle
       write(str(9),'(F13.8)') self%theta
       write(str(10),'(I5)') self%stepDuration
       write(str(11),'(A)') get_TimeFromMinutes(self%time_of_day)
       write(str(12),'(I9)') self%nbrTrials
      ! write(str(7),'(F5.1)')  self%distStartNest ! abs(distance(real(self%coord), realstartNestCoordinates))
       if (self%treeIndex .gt. 0) then 
          atree = get_tree(self%treeIndex)
          write(str(13),'(I3)') atree%treeID
          write(str(14),'(L)') self%Swallow
          write(str(15),'(L)') self%spitted 
       else 
          str(13:15) = ''
       end if
       if (self%nestIndex .gt. 0) then 
          write(str(16),'(A)') 'T'
       else 
          str(16) = ''
       end if
       if (self%end_of_day) then 
          write(str(17),'(A)') 'EOD'
       else 
          str(17) = ''
       end if
      display_point = ''

      write(display_point,'(34A)') (trim(adjustl(str(i))),tab,i=1,17)
     
 end function display_point 

!===================================================================================
character(256) function display_header()
!--------------------------------------------------------------------------------
   write(display_header,'(33A)') 'run#',tab,'day',tab,'point#',tab,'state',tab,'X',tab,'Y',tab,'Step', &
                   tab,'von_Mises', &
                   tab,'Angle',tab,'Duration',tab,'Time',tab,'trials#',tab,'Tree ID',tab,'swallow', &
                   tab,'spitted',tab,'Nest',tab,'End of day'
   
 end function display_header 

!===================================================================================
LOGICAL function SlopeBetweenOthers(slope,others) 
      real,  intent(in) :: slope
      real,  dimension(2), intent(in) :: others

      if (others(2) .GT. others(1)) then
          ! horizontal line at 0 degrees is between both slopes
         SlopeBetweenOthers = (slope .LE. others(1) .OR. slope .GE. others(2) ) 
      else 
          ! both slopes are greater then 0 degrees
         SlopeBetweenOthers = (slope .LE. others(1) .AND. slope .GE. others(2))
      end if
     

END FUNCTION SlopeBetweenOthers

!===================================================================================
FUNCTION PassThroughTree(self, previousPoint, treeCoord, radius ) RESULT(YesNo)
!! Could the step pass in the surrounding of tree or bed
!! self is the next point with new rho and theta 
        class (t_point), intent(in) :: self, previousPoint
        integer(kind = kCoord),dimension(2), intent(in) :: treeCoord
        integer, intent(in) :: radius
        LOGICAL :: YesNo
        
        
        real :: realRadius, distance_nextpoint_Tree
        real, dimension(2) :: realTreeCoord
        real,  dimension(3) :: slopes
        
        YesNo = .FALSE.
        realRadius = REAL(radius)
        realTreeCoord = REAL(treeCoord)
        
        if (distance(self%realCoord, realTreeCoord) .LE. realRadius) THEN
            ! The next point is in the surrounding of the tree
            ! We keep the tree coord as the next point
            YesNo = .TRUE.
            RETURN
        end if
        if (distance(previousPoint%realCoord, realTreeCoord) .LE. realRadius) THEN
            ! The previous point is already at the tree
            ! We bypass that tree to allow the step to be done
            YesNo = .FALSE.
            RETURN
        end if
      
        if (self%rho < distance(previousPoint%realCoord, realTreeCoord) - realRadius) THEN
          ! the step rho is too small to reach the tree, the step doesn't change
            YesNo = .FALSE.
            RETURN
        END IF
        
        slopes = previousPoint%SlopesSurroundingSecondPoint(realTreeCoord, radius )
        if(SlopeBetweenOthers(self%theta,slopes(1:2)) ) THEN
          YesNo = .TRUE.  !! we pass through the surrounding of the tree
                           !! We keep the tree coord as the next point
        end if
        
          
        

END FUNCTION PassThroughTree
!===================================================================================
function  SlopesSurroundingSecondPoint(self, otherPoint, radius ) RESULT (w)
!--------------------------------------------------------------------------------
     class (t_point), intent(in) :: self
     real,dimension(2), intent(in) :: otherPoint
     integer, intent(in) :: radius
     real,  dimension(3) :: w
     
     real,dimension(2) :: proximalPoint, leftPoint, rightPoint
     type(t_edge) :: betweenPoints
     real :: triangleLongSide
     real :: slope, leftSlope, rightSlope
     real :: realRadius
    character(128) :: msg 
     
    integer :: i
      
     realRadius = REAL(radius)
 !    WRITE(*,'(A,F14.7,A,F14.7)') 'self point X: ', self%realCoord(X), ' Y: ',self%realCoord(Y)
 !         WRITE(*,'(A,F14.7,A,F14.7)') 'other point X: ', otherPoint(X), ' Y: ',otherPoint(Y)
  !   WRITE(*,'(A,F14.7)') "DISTANCE", distance(self%realCoord, otherPoint)    
     if (distance(self%realCoord, otherPoint) .LE. realRadius) then
          write(msg,'(A,F7.2,A,I3)') 'In function "SlopesSurroundingSecondPoint()", distance between points ',  &
          distance(self%realCoord, otherPoint), '   is .LE. than radius', radius
          call fatal_error(msg)
     end if
     

 
     triangleLongSide = sqrt(2.0 * (realRadius ** 2)) ! long côté du triangle rectangle
!    write (*,'(2A,F7.4)') 'Long côté : ', tab, triangleLongSide
!    write(*,'(3A,I3)') 'Radius:',tab,tab,radius
 
   
      slope = slope_between_points_REAL(self%realCoord,otherPoint)
      leftSlope = slope + angle45 ! 45 degrés est angle aigu d'un triangle rectangle isocèle
      rightSlope = slope - angle45
     

     betweenPoints%startP = otherPoint
     betweenPoints%endP = self%realCoord
     
      ! proximal est le point à l'intersection du cercle de rayon radius autour du point "otherPoint" et du segment entre self et otherpoint
      proximalPoint = point_at_dist(betweenPoints, realRadius)
   !   write (*,'(A,F14.7,X,F14.7)') 'Proximal : ', proximalPoint

      ! leftPoint est le point à gauche au-dessus sur le cercle de rayon radius autour du point "otherPoint" 
      leftPoint(X) = proximalPoint(X)+COS(leftSlope)*realRadius
      leftPoint(Y) = proximalPoint(Y)+SIN(leftSlope)*realRadius
      
      ! rightPoint est le point à droite en-bas sur le cercle de rayon radius autour du point "otherPoint" !
      rightPoint(X) = proximalPoint(X)+COS(rightSlope)*realRadius
      rightPoint(Y) = proximalPoint(Y)+SIN(rightSlope)*realRadius
      
   !   write(*,'(A)') '-----------'
   !   WRITE(*,'(A,F14.7,A,F14.7)') 'Left point X: ', leftPoint(X), ' Y: ',leftPoint(Y)
    !  WRITE(*,'(A,F14.7,A,F14.7)') 'Right point X: ', rightPoint(X), ' Y: ',rightPoint(Y)
   !   write(*,'(A)') '--------------'
      w(1) = slope_between_points_REAL(self%realCoord,leftPoint)
      w(2) = slope_between_points_REAL(self%realCoord,rightPoint)
      w(3) = slope ! la pente vers l'autre point

  end function SlopesSurroundingSecondPoint 
!===================================================================================
 
 !==============================================================   
INTEGER FUNCTION time_of_week (self )
  ! time since beginning of week
!--------------------------------------------------------------
    class (t_point), intent(in) :: self
    
    ! 1440 = 24*60 minutes in a day
    time_of_week = ((self%dayNumber-1) * 1440) + self%time_of_day

END FUNCTION time_of_week 



 !==============================================================   
 subroutine TestCalculsPoints()
      type (t_point) :: firstPoint, nextPoint
      real :: pente
      real,  dimension(3) :: pentes
      integer(kind = kCoord),dimension(2) :: treeCoord
      
      integer :: err, radius,i
      double precision :: dblevar
      LOGICAL :: SlopeBetween, AtTree
      REAL:: NewTheta
      character(100) :: str
         type(t_tree) :: theTree
         
      call firstPoint%init();
      call nextPoint%init();
      
      open(unit=1,file='./testpoint.txt',status='old', iostat=err)
      if(err /= 0) then
         str = "File testpoint.txt not found."
         call fatal_error(str)
         return
      end if
      call get_IntegerParamArray(1,firstPoint%coord)
      call get_IntegerParamArray(1,treeCoord)
      radius = get_IntegerParam(1)
      nextPoint%rho = real(get_DoubleParam(1))
      dblevar = get_DoubleParam(1)
      nextPoint%theta = real(dblevar)
      close(1) 
        firstPoint%realCoord = real(firstPoint%coord)
        nextPoint%coord(X) = firstPoint%coord(X) + (nextPoint%rho * cos(nextPoint%theta))
        nextPoint%coord(Y) = firstPoint%coord(Y) + (nextPoint%rho * sin(nextPoint%theta))
        nextPoint%realCoord = real(nextPoint%coord)


 write(*,'(A)') '***********************************'
          WRITE(*,'(2(A,I7))') 'start point: ', firstPoint%coord(X), ',',firstPoint%coord(Y)
         WRITE(*,'(A,I7,A,I7)') 'tree coord:   ', treeCoord(X), ',',treeCoord(Y)
         write (*,'(A,F18.7)') "Distance first point à l'arbre : ", distance(firstPoint%realCoord, real(treeCoord))
         WRITE(*,'(2(A,I7))') 'next point: ', nextPoint%coord(X), ',',nextPoint%coord(Y)
         write (*,'(A,F18.7)') "Distance first point à next point : ", distance(firstPoint%realCoord, nextPoint%realCoord)
         write (*,'(A,F18.7)') "Distance next point à l'arbre : ", distance(nextPoint%realCoord, real(treeCoord))
         WRITE(*,'(A,I7)') 'radius: ', radius
         WRITE(*,'(A,I7)')  "rayon à l'arbre paramètre: ", prm_i_TreeRadius
         if (distance(firstPoint%realCoord, real(treeCoord)) .GT. real(Radius)) then
             pentes = firstPoint%SlopesSurroundingSecondPoint(real(treeCoord),radius)
         end if

        WRITE(*,'(A,A,F14.7,X,I7,A)') 'Left slope: ', tab, pentes(1), Radian_to_Degrees(pentes(1)),' degrés'
        WRITE(*,'(A,A,F14.7,X,I7,A)') 'slope first point to tree:      ', tab, pentes(3), Radian_to_Degrees(pentes(3)),' degrés'
        WRITE(*,'(A,A,F14.7,X,I7,A)') 'Right slope: ', tab, pentes(2), Radian_to_Degrees(pentes(2)),' degrés'
 write(*,'(A)') '--------------' 
      WRITE(*,'(A,A,F14.7,X,I7,A)') 'nextPoint%theta:      ', tab, nextPoint%theta, Radian_to_Degrees(nextPoint%theta),' degrés' 
      WRITE(*,'(A,A,F14.7)') 'nextPoint%rho:      ', tab, nextPoint%rho
      if (distance(real(treeCoord), nextPoint%realCoord) .GT. real(radius)) then
        SlopeBetween = SlopeBetweenOthers(nextPoint%theta,pentes) 
        WRITE (*,'(A, L)') "Pente dans l'angle ",  SlopeBetween
      else
        WRITE (*,'(A)') "Next point est dans le cercle de l'arbre"
      end if
      AtTree = nextPoint%PassThroughTree(firstPoint,treeCoord, radius ) 
     WRITE (*,'(A, L)') "At tree ",  AtTree
       write(*,'(A)') '================================'  
         write(*,'(6A,25X,3A)') 'Idx   X', tab,tab, 'Y',tab,'Name','Swallow',tab,'Spitted'
       do i=1,size_of_trees()
             theTree = get_tree(i)
             AtTree = nextPoint%PassThroughTree(firstPoint, theTree%coord, prm_i_TreeRadius )
           if (AtTree)                then
           write (*,'(L,1X,I3,I7,2X,I7,3A,L,A,L)') AtTree, TheTree%treeID,TheTree%coord(X),TheTree%coord(Y), &     
                tab, TheTree%tree_name(1:30),tab,TheTree%swallow,tab,TheTree%spitted
           end if
          end do  
 end subroutine TestCalculsPoints 
!**************************************************
 end module m_point
!**************************************************  
