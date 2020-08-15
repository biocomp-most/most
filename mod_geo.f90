

!******************************************
! Geometric calculations
!******************************************

module m_geo
  
  use m_parameters
  use m_tools
  
  implicit none
  private 
  
  integer,parameter :: x = 1
  integer,parameter :: y = 2
    
  type t_vertex
    integer(kind = kCoord),dimension(2) :: point
    integer :: Id
  end type t_vertex
  
   type t_edge
    real,dimension(2) :: startP ! start point
    real,dimension(2) :: endP ! end point
   end type t_edge
  
  type t_polygon
     type(t_vertex),dimension(:),allocatable :: Vertices
	 integer(kind = kCoord) :: topY, bottomY, leftX, rightX
     character(40) :: filename 
     
   contains  
     procedure,pass(self),private :: read_polygon
     procedure,pass(self),public :: toString => print_polygon   
     procedure,pass(self),public :: containsPoint => is_point_in_polygon
     
  end type t_polygon
  
  type(t_polygon),public,save :: NoGoPolygon
  type(t_polygon),public,save :: HRPolygon
 ! type(t_polygon),public,save :: KernelPolygon
  
  public :: t_edge
  public :: create_polygons
  public :: print_polygons
  public :: deallocate_polygons
  public :: check_point_at_dist
  public :: is_PointInKernel
  public :: distance
  public :: point_at_dist
  public :: slope_between_points_REAL
  
  
CONTAINS
  
!=====================================================
     subroutine read_polygon(self, fileName,NoGoFile)
!-- read polygon points from text file 
!-----------------------------------------------------    
      class (t_polygon), intent(out)  :: self
      character(*), intent(in) :: fileName
      logical,intent(in)::NoGoFile
      character(80) :: filePath
      
      integer :: err, i, nbrVertices
      character(160) :: str
	  type(t_vertex) :: firstV, lastV
      character(50),dimension(6) :: splitted_str
            
      nbrVertices = 0
      self%fileName = fileName
      
      filePath = create_filepath(prm_c_input_dir, filename)
      !// Count number of vertices
      open(unit=1,file=filePath,status='old', iostat=err)
      if(err /= 0) then
         str = "File " // trim (filePath) // " not found."
         call fatal_error(str)
         return
      end if
      read (unit=1,fmt=*,iostat=err) str !! first line
      do while (err == 0) 
         read (unit=1,fmt=*,iostat=err) str !! first line
         if (err /= 0) then; exit; end if
         nbrVertices = nbrVertices + 1 
      end do
      close(1) 

      !// read vertices 
      allocate(self%Vertices(nbrVertices))
 !!     write(*,*) nbrVertices
!! reopen file after having allocated array of vertices 
      open(unit=1,file=filePath,status='old', iostat=err)
      read (unit=1,fmt=*,iostat=err) str !! first line
      if( NoGoFile ) then
         read (1,'(i6,1x,i7,1x,i3)') self%Vertices !! read complete array       
      else
loop2:     do i=1,nbrVertices
        read (unit=1,fmt='(A)',iostat=err) str !! data line
       call split_string(trim(str),splitted_str,';')
       READ(splitted_str(2), '(I)') self%Vertices(i)%point(X)
       READ(splitted_str(3), '(I)') self%Vertices(i)%point(Y)
       READ(splitted_str(1), '(I)') self%Vertices(i)%Id
      end do loop2
      end if 
      close (1)

      self%topY = maxval(self%Vertices%point(Y))
      self%bottomY = minval(self%Vertices%point(Y))
      self%leftX = minval(self%Vertices%point(X))
      self%rightX = maxval(self%Vertices%point(X))
      

      firstV = self%Vertices(1)
      lastV = self%Vertices(nbrVertices)
      if(firstV%point(X) /= lastV%point(X) .OR. firstV%point(Y) /= lastV%point(Y)) then
         str = "In file " // trim (filename) // ", last point coordinates not equal to first point coordinates."
         call fatal_error(str)
         return
      end if
      
    end subroutine read_polygon
    
!=====================================================
   subroutine print_polygon(self)   
!-----------------------------------------------------      
       class(t_polygon),intent(in) :: self
       write (*,*) "---- Polygon " // self%filename
       write (*,"('Left X:', I8, ' Right X: ', I8)"), self%leftX, self%rightX
       write (*,"('Top Y: ', I8, ' Bottom Y:', I8)"), self%topY, self%bottomY
       write (*,*) ""
       write (*,*) '  X       Y       Id'
       write(*,'(1X,I6,2x,i7,2x,i3)') self%vertices
       write (*,*) "================================"
   end subroutine print_polygon
   
    
!=====================================================
   subroutine print_polygons()
!-----------------------------------------------------    
        call NoGoPolygon%toString()
        call HRPolygon%toString()
      !  call KernelPolygon%toString()
   end subroutine 

!=====================================================   
    subroutine create_polygons()
!-----------------------------------------------------     
      implicit none
       call NoGoPolygon%read_polygon(prm_c_NoGoFile,.TRUE.)
       call HRPolygon%read_polygon(prm_c_MCPFile(prm_i_month),.FALSE.)
      ! call KernelPolygon%read_polygon(KernelFile)
       ! call print_polygons()
      ! call KernelPolygon%toString()
     end subroutine create_polygons   
     
!=====================================================   
    subroutine deallocate_polygons()
!-----------------------------------------------------     
      implicit none
       ! deallocate (NoGoPolygon%vertices, HRPolygon%vertices, KernelPolygon%vertices)
        deallocate (NoGoPolygon%vertices, HRPolygon%vertices)
     end subroutine deallocate_polygons
     
     
!=================================================
    function is_point_left(P0,P1,P2) 
!=================================================      
!/ isLeft(): tests if a point is Left|On|Right of an infinite line.
!//    Input:  three points P0, P1, and P2
!//    Return: >0 for P2 left of the line through P0 and P1
!//            =0 for P2  on the line
!//            <0 for P2  right of the line
!//    See: Algorithm 1 "Area of Triangles and Polygons"
    integer(kind = kCoord),dimension(2),intent(in) :: P0,P1,P2
    integer :: is_point_left
    
    is_point_left = ( (P1(x) - P0(x)) * (P2(y) - P0(y)) &
            - (P2(x) -  P0(x)) * (P1(y) - P0(y)) )
    end function is_point_left  

!=================================================
    function is_point_in_polygon(self, P) 
!=================================================    
   ! base on  "Winding Number" algorithm
   ! see -> http://geomalgorithms.com/a03-_inclusion.html#wn_PnPoly()
    !     Input:   P = a point,
    !     polygon to searched  V[] = vertex points of a polygon V[n] with V[n]=V[1]
    !     Return:  wn = the winding number (==0 only when P is outside)
      implicit none
      class(t_polygon),intent(in) :: self
      integer(kind = kCoord),dimension(2),intent(in) :: P

      logical :: is_point_in_polygon
      
      integer :: i,n, wn;    ! the  winding number counter
      n = size(self%vertices)
      wn = 0
      do i=1,n-1 !// edge from V[i] to  V[i+1]
         if (self%vertices(i)%point(y) .le. P(y)) then ! start y <= P.y
          if (self%vertices(i+1)%point(y) .gt. P(y)) then !// an upward crossing
           if (is_point_left(self%vertices(i)%point, &
               self%vertices(i+1)%point, P) .gt. 0) then ! // P left of  edge
                wn = wn + 1 ! // have  a valid up intersect
           end if
          end if
         else ! start y > P.y (no test needed)
          if (self%vertices(i+1)%point(y) .le. P(y)) then !// a downward crossing
           if (is_point_left(self%vertices(i)%point, &
               self%vertices(i+1)%point, P) .lt. 0) then ! // P right of  edge
                wn = wn - 1 ! // have  a valid down intersect
           end if
          end if
         end if
      
      end do
      
     
      is_point_in_polygon = (wn /= 0)
    end function is_point_in_polygon




!=================================================
    function is_PointInKernel(pt) 
!=================================================    
     implicit none
      integer(kind = kCoord),dimension(2),intent(in) :: pt
      logical :: is_PointInKernel
      
      is_PointInKernel = .false.
        if (HRPolygon%containsPoint(pt)) then
          if (.not. NoGoPolygon%containsPoint(pt)) then

           is_PointInKernel = .true.
        end if
      end if
      
         
      ! is_PointInKernel = .not. NoGoPolygon%containsPoint(pt) & 
                     ! .and. HRPolygon%containsPoint(pt)
    end function is_PointInKernel
    

!=================================================
    function distance(P1, P2)
!=================================================    
     implicit none
      real ,dimension(2),intent(in) :: P1,P2
      real :: distance
      
      if(P1(X)==P2(X) .AND. P1(Y)==P2(Y)) then
         distance = 0.0
      else 
        distance = SQRT(SUM((P1 - P2) ** 2))
      end if

      end function distance
      
!=================================================
    function middle_point(P1, P2)
!=================================================    
     implicit none
      real,dimension(2),intent(in) :: P1,P2
      real,dimension(2) :: middle_point

      middle_point = (P1 + P2) / 2.0

      end function middle_point


!=================================================
    function point_at_dist(edge, dist)
!=================================================    
! http://math.stackexchange.com/questions/175896/finding-a-point-along-a-line-a-certain-distance-away-from-another-point
! http://math.stackexchange.com/questions/409689/how-do-i-find-a-point-a-given-distance-from-another-point-along-a-line

     implicit none
      type(t_edge), intent(in) :: edge
      real,intent(in) :: dist
      real,dimension(2) :: point_at_dist
      character(128) msg 
      
      real:: m

  !    m = (P2(Y)-P1(Y)) / (P2(X)-P1(X)) ! gradient
   !   point_at_dist(X) = P1(X) +- sqrt(dist**2/(1+m**2))
  !    point_at_dist(Y) = m * (point_at_dist(X)-P1(X)) + P1(Y) 
  
 
       if (edge%endP(X) .EQ. edge%startP(X)) then
         if (edge%endP(Y) .EQ. edge%startP(Y)) then
          write(msg,'(A,F10.0,A,F10.0)') 'In function "point_at_dist()", the start and end vertex of the edge are equal:', &
            edge%startP(X),',',edge%startP(Y)
          call fatal_error(msg)
         end if
       ! vertical line
           if(edge%endp(Y) > edge%startp(Y)) then
             point_at_dist(Y) = edge%startp(Y) + dist
           else
             point_at_dist(Y) = edge%startp(Y) - dist           
           end if
           point_at_dist(X)  = edge%startp(X)
           
       else if (edge%endP(Y) .EQ. edge%startP(Y)) then
       ! horizontal line 
           if(edge%endp(X) > edge%startp(X)) then
             point_at_dist(X) = edge%startp(X) + dist
           else
             point_at_dist(X) = edge%startp(X) - dist           
           end if
           point_at_dist(Y)  = edge%startp(Y)
       
       else 
          m = (edge%endP(Y)-edge%startP(Y)) / (edge%endP(X)-edge%startP(X)) ! gradient
          
          point_at_dist(X) = edge%startP(X) + sqrt(dist**2/(1+m**2))
          if (point_at_dist(X) > edge%endP(X)) then
            ! if calculated point is outside the edge, we change sign to keep it within the edge 
            point_at_dist(X) = edge%startP(X) - sqrt(dist**2/(1+m**2))
          end if
          point_at_dist(Y) = m * (point_at_dist(X)-edge%startP(X)) + edge%startP(Y) 
       end if
 end function point_at_dist   

!=================================================      
    SUBROUTINE check_point_at_dist(step)      
!=================================================
     real, intent(in) :: step
     
     REAL,dimension(2) :: P1,P2 ,P3
     real :: d12,d13,R4
     
     integer :: i,n
     character(*),parameter :: frm = '(F11.4,A1,F12.4,A1,A)'
     character(80) :: outfile
     character(20) :: str
     type(t_edge) :: e
          
      p1 = (/484937.0,8309440.0/)
      p2 = (/485647.0,8310110.0/)
      
      d12 = distance(p1,p2)
      n = (d12/step)+1
      e%startP =P1
      e%endP = P2

        
      outfile = create_filepath(prm_c_output_dir, 'check_point_distance.txt')
      open(1,file=outfile,status='replace',action='write')
      write (1,'(4A1,A)') "X",tab,"Y",tab,"Id"
      write (1,frm) P1(X),tab,P1(Y),tab,"1"

      do i=2,n
       P3 = point_at_dist(e, (i-1)*step)
       write(str,*) i
       write (1,frm) P3(X),tab,P3(Y),tab,adjustl(str)
      enddo 
         write(str,*) n+1
      write (1,frm) P2(X),tab,P2(Y),tab,adjustl(str)
      close(1)
    
    END SUBROUTINE check_point_at_dist

!=====================================
real function slope_between_points_REAL(from_point,to_point)
!=================================================
    ! returns angle between 0 and 2PI radians from horizontal line passing at
    !  from_point  

           !! see : http://res-nlp.univ-lemans.fr/NLP_C_M01_G01/co/contenu_24.html
           !! sin(theta) = y / rho 
           !! donc theta = ASIN(Y/rho)  rho est le grand côté du triangle rectangle
           
    real,dimension(2),intent(in) :: from_point,to_point
    real triangle_height, dist_between_points
 
     triangle_height = ABS(to_point(Y) - from_point(Y))
     dist_between_points = abs(distance(from_point, to_point))

     slope_between_points_REAL = ASIN(ABS(triangle_height / dist_between_points))
     if (to_point(Y) .lt. from_point(Y)) then
                if(to_point(X) .lt. from_point(X) ) then
                  ! quart inférieur gauche
                  slope_between_points_REAL = real_PI + slope_between_points_REAL
                else
                  ! quart inférieur droit
                  slope_between_points_REAL = real_PI * 2.0 - slope_between_points_REAL
                end if
           else 
                if(to_point(X) .lt. from_point(X) ) then
                  ! quart supérieur gauche
                  slope_between_points_REAL = real_PI - slope_between_points_REAL
               end if
      end if

end function slope_between_points_REAL      
    
 

end module m_geo



