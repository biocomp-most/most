!**************************************************
 module m_defecation
!**************************************************

  use m_common
  use m_step
  use m_angle
  use m_tools
  use m_point
  use m_geo
  use m_daytime
  use m_path
  use m_point
  use m_transit
  use m_trees
  

  implicit none
  
  
  type t_defecation
    integer :: weekIndex 
    integer :: dayNumber ! 1:7
    integer :: point_idx
    integer :: treeIndex
    integer(kind = kCoord),dimension(2) :: tree_coord
    integer(kind = kCoord),dimension(2) :: defecation_coord
    real,dimension(2) :: realTree_Coord,realdefecation_coord
    integer ::  transit_duration
    integer :: time_of_swallow
    real :: traveled_dist
    real :: shortest_dist
    character(200) :: note  
    integer :: timeOfWeek   ! time since beginning of week
    integer(kind = kCoord),dimension(2) :: P1_coord
    integer(kind = kCoord),dimension(2) :: P2_coord
    character(26) :: P1_idx, P2_idx
    
  contains
    procedure,pass(self),public  :: init_defecation
    procedure,pass(self),public :: display_defecation_point
    procedure,pass(self),public :: format_defecation_point_idx


  end type t_defecation
  
  
  CONTAINS
!=================================================================================== 
character(16) function format_defecation_point_idx(self)
     class (t_defecation), intent(in) :: self
   
   character(16),dimension(5) :: strIdx
   integer :: i
      
      write(strIdx(1),*) self%dayNumber
      strIdx(2) = "-"
      write(strIdx(3),*) self%point_idx
      strIdx(4) = "-"
      strIdx(5) = "T"
       
    write(format_defecation_point_idx,'(5A)') (trim(adjustl(strIdx(i))),i=1,5)
    
end function format_defecation_point_idx
!==============================================================   
  subroutine init_defecation (self, treePoint)
      class (t_defecation), intent(out) :: self
      type (t_point) , intent(in) :: treePoint
      self%weekindex = treePoint%weekindex 
      self%dayNumber = treePoint%dayNumber
      self%point_idx = treePoint%idx
      self%treeIndex = treePoint%treeIndex
      self%tree_coord = treePoint%coord
      self%realTree_Coord = REAL(self%tree_coord)
      self%defecation_coord = 0
      self%realdefecation_coord = 0.0
      self%transit_duration = 0
      self%time_of_swallow = treePoint%time_of_day
      self%traveled_dist = 0.0
      self%shortest_dist = 0.0
      self%timeOfWeek = treePoint%time_of_week()
      self%note = ''
      self%P1_coord = 0
      self%P2_coord = 0
      self%P1_idx = ''
      self%P2_idx = ''
      
      
  end subroutine init_defecation
!==============================================================   
 

 
!===================================================================================
character(512) function display_defecation_point(self)
!--------------------------------------------------------------------------------
      class (t_defecation), intent(in) :: self

      character(16),dimension(18) :: str
      integer :: i
      type(t_tree) :: atree
      character(10) :: outfmt
      
      str = ''
      
   
      
       write(str(1),*) self%weekIndex
       write(str(2),*) self%dayNumber
       str(3) = self%format_defecation_point_idx()
       write(str(4),'(I)') self%tree_coord(X)
       write(str(5),'(I)') self%tree_coord(Y)
       write(str(6),'(A)') get_TimeFromMinutes(self%time_of_swallow)
       atree = get_tree(self%treeIndex)
       write(str(7),'(I3)') atree%treeID
       if (self%defecation_coord(X) .NE. 0) then
            write(str(8),'(I)') self%defecation_coord(X)
            write(str(9),'(I)') self%defecation_coord(Y)
       end if 
       write(str(10),'(I)') self%transit_duration
       if (self%defecation_coord(X) .NE. 0) then
           write(str(11),'(I)') NINT(self%traveled_dist)
           write(str(12),'(I)') NINT(self%shortest_dist)
       end if
       str(13) = self%P1_idx
       if(str(13).NE.'') then
           write(str(14),'(I)') self%P1_coord(X)
           write(str(15),'(I)') self%P1_coord(Y)
       end if
       str(16) = self%P2_idx
       if(str(16).NE.'') then
           write(str(17),'(I)') self%P2_coord(X)
           write(str(18),'(I)') self%P2_coord(Y)
       end if
       display_defecation_point = ''
       if(prm_l_dspDefecationDetails) then
          write(outfmt,'(A,I2,A)') '(', size(str)*2 + 1, 'A)'
          write(display_defecation_point,outfmt) (trim(adjustl(str(i))),tab,i=1,size(str)),self%note
       else 
          write(outfmt,'(A,I2,A)') '(', (size(str)-4)*2 + 1, 'A)'
          write(display_defecation_point,outfmt) (trim(adjustl(str(i))),tab,i=1,size(str)-5), trim(str(16)), tab, self%note
       
       end if
     
 end function display_defecation_point 

!===================================================================================
character(256) function display_defecation_header(is_defecation)
!--------------------------------------------------------------------------------
   logical,intent(in) :: is_defecation
   character(20) :: x_header
   character(20) :: y_header
   if(is_defecation) then
       write(x_header,'(A,X,A)') 'X', 'defecation'
       write(y_header,'(A,X,A)') 'Y', 'defecation'
   else 
      write(x_header,'(A,X,A)') 'X', 'spitted'
      write(y_header,'(A,X,A)') 'Y', 'spitted'
   endif
   if(prm_l_dspDefecationDetails) then
       write(display_defecation_header,'(37A)') 'run#',tab,'day',tab,'point#',tab,'tree X',tab,'tree Y',tab,'Time of swallow',tab,'Tree ID', &
                tab,x_header,tab, y_header, tab, 'transit duration',tab,'traveled distance',tab,'shortest distance', &
                tab,'P1',tab, 'P1 X', tab, 'P1 Y', tab,'P2',tab, 'P2 X', tab, 'P2 Y',  tab, 'note'
   else
       write(display_defecation_header,'(31A)') 'run#',tab,'day',tab,'point#',tab,'tree X',tab,'tree Y',tab,'Time of swallow',tab,'Tree ID', &
                tab,x_header,tab, y_header, tab, 'transit duration',tab,'traveled distance',tab,'shortest distance', &
                tab,'ejection between', tab,'and', tab, 'note'
   
   end if
   
 end function display_defecation_header 

!===================================================================================

!**************************************************
 end module m_defecation
!**************************************************  
