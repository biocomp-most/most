!**************************************************
module m_nest

!! Reads sleeping sites in sleeping_sites.txt file and store them in an array of t_nest objects
!!  Provides randomly a new nest in get_NewNest() function depending on coordinates
!!
!**************************************************
  use m_common
  use m_tools
  use m_geo
  
  implicit none

  private 
  integer, parameter :: X = 1
  integer, parameter :: Y = 2
  
  type t_nest
     integer(kind = kCoord),dimension(2) :: coord
     real(8) :: percent
     real(8) :: sumPercent
  end type t_nest
  
  
  type(t_nest), dimension(:), save, allocatable :: nests
 
  
  public :: t_nest

  public :: create_nests
  public :: deallocate_nests   
  public :: print_nests

  public :: get_NewNest
  public :: test_NewNest
  public :: get_nest
  public :: get_nearest_nest_index
  public :: size_of_nests
  

  
  CONTAINS
  
!=================================================   
   subroutine create_nests()
!=================================================   
    use m_tools
    implicit none
    
      character(80) :: filePath
      
      integer :: err, i,nbr_records, nbr_names,mm
      integer(kind=kCoord) :: xc, yc
      double precision :: percent, sumPercent
 
      character(5),dimension(200) :: names
      character(160) :: str
      character(50),dimension(2) :: splitted_name
      character(10) :: tree_type
      character(50),dimension(4) :: splitted_str
            character(80) :: textFile
            
	  
      nbr_records = 0
      nbr_names = 0

      textFile = prm_c_SleepingSitesFile
      filePath = create_filepath(prm_c_input_dir, prm_c_SleepingSitesFile)
      !// Count number of records
      open(unit=1,file=filePath,status='old', iostat=err)
      if(err /= 0) then
         str = "File " // trim (filePath) // " not found."
         call fatal_error(str)
         return
      end if
      read (unit=1,fmt=*,iostat=err) str !! header line
while1:   do while (err == 0) 
        read (unit=1,fmt=*,iostat=err) str !! data line
       if (err /= 0) then; exit; end if
         mm = stringMonth_to_integer(str(1:3), textFile)
         if (mm .EQ. prm_i_month) then
              nbr_records = nbr_records + 1 
         end if
         end do while1
      close(1)    
      ! write (*,*) nbr_records, nbr_Groups
 
      allocate(nests(nbr_records))  

      ! reopen file after having allocated array 
      open(unit=1,file=filePath,status='old', iostat=err)
      read (unit=1,fmt=*,iostat=err) str !! header line
       loop2:     do i=1,nbr_records
 loop3:     do while (err == 0)
           read (unit=1,fmt='(A)',iostat=err) str !! data line
           mm = stringMonth_to_integer(str(1:3), textFile)
           if (mm .EQ. prm_i_month) then
              exit loop3
           end if
        end do loop3
      sumPercent = 0.0
     !   read (unit=1,fmt='(A)',iostat=err) str !! data line
     !   write(*,'(1A)') str
      !  read (unit=1,fmt='(I6,1X,I7,1X,G)',iostat=err) xc,yc,percent !! data line
        call split_string(trim(str(8:)),splitted_str,tab)
       ! READ(splitted_str(1), '(I)') trees(i)%treeID
        READ(splitted_str(2), '(I)') nests(i)%coord(X)
        READ(splitted_str(3), '(I)') nests(i)%coord(Y)
        READ(splitted_str(4), '(G)') percent
     !   write(*,'(2A,I7,2A,I7,2A,G)') 'xc:', tab, xc, ' yc:',tab, yc, ' percent:', tab, percent
        sumPercent = sumPercent + percent 
        nests(i)%sumPercent = sumPercent / 100.0D0
        nests(i)%percent = percent
      end do loop2
      close(1)  
            

      
     end subroutine create_nests    
     
!================================================= 
      subroutine deallocate_nests()
!=================================================      
      implicit none
      deallocate (nests)
     end subroutine deallocate_nests 
     
  
!================================================= 
      subroutine print_nests()
!=================================================      
      implicit none
      integer :: i
      ! type(t_tree) :: ATree
      ! real,dimension(2) :: pt 
      write(*,*) '****************************************'
      write(*,*) 'Nest coordinates'
      write(*,*) '-----------'
      write(*,'(8A)') 'Idx   X', tab,tab, 'Y',tab,'    Sum %'
   !   write (*,'(I6,1X,I7,2(1X,A))') (nests(i)%p(1),nests(i)%p(2), nests(i)%tree_name(1:30), nests(i)%tree_code,i=1,size(nests))
      write (*,'(I3,2X,I6,2X,I7,2X,F11.8)') (i, nests(i)%coord(1),nests(i)%coord(2),nests(i)%sumPercent, &
                         i=1,size(nests))
      
      ! pt(X) = 485545.6 
      ! pt(Y) = 8311000.2
      ! ATree = nearest_tree(pt)
 
      ! write(*,*) PT 
      ! write(*,*) ATree 
      ! call test_NewNest()
      
     end subroutine print_nests    

!===================================================================================
    subroutine get_NewNest(coord,i)
!--------------------------------------------------------------------------------
     integer(kind = kCoord),dimension(2), intent(out) :: coord
      integer, intent(out) :: i
      real(8)  :: rndNumber
         

      logical :: found
     
      found = .FALSE.
      
      do while (.NOT. found)
        
          call random_number(rndNumber)
          
          do i = 1, size(nests)
             if (rndNumber < nests(i)%sumPercent) then
              !   if (is_PointInKernel(nests(i)%coord)) then
                    found = .TRUE.
                    coord = nests(i)%coord
              !   end if
                 exit 
             end if
          end do 
      
      end do
      
 end subroutine get_NewNest
 !===================================================================================
    subroutine test_NewNest()
!--------------------------------------------------------------------------------
     integer(kind = kCoord),dimension(2) :: coord
      integer :: j
      integer(8) :: i, imax
    !  real(8) :: rnd
      integer(8), dimension(size(nests)) :: compteur  
      character(100) :: outfile
      write (outfile, '(3A)') trim(prm_c_output_dir), 'TestNewNest', '.txt'
      compteur = 0
      do i = 1, 100000000
        
          
          call get_NewNest(coord,j)
       !   write(*,'(I4,2X,I7,2X,I7,2x,I4,2X,F11.8)') i,coord,j !! ,rnd
          compteur(j) = compteur(j)+1
           
      
      end do
      
      open(1,file=outfile,status='replace',action='write')
     write (1,'(3A)') "Id",tab,"Nbr"
          do j = 1, size(nests)
          write (1,'(I3,A,I9,2(A,F11.8),A,F6.3)') j,tab,compteur(j),tab,compteur(j)/1000000D0,tab,nests(j)%percent,tab,nests(j)%percent-compteur(j)/1000000D0
          end do
  
       close(1) 
  
 end subroutine test_NewNest
 ! !===================================================================================
  type(t_nest) function get_nest(idx)
   integer, intent(in) :: idx
   get_nest = nests(idx)
  end function get_nest
  
  integer function size_of_nests()
      size_of_nests = size(nests)
  end function size_of_nests

 ! !===================================================================================
  integer function get_nearest_nest_index(pointCoord)
  real,dimension(2), intent(in) :: pointCoord
  double precision :: shortestDistance, dist
  integer :: i
   
   shortestDistance = 999999999.0D0
   get_nearest_nest_index = 0
   do i = 1,  size(nests)
     dist=distance(pointCoord, real(nests(i)%coord))
     if (dist .LT. shortestDistance) then
        shortestDistance = dist
        get_nearest_nest_index=i 
     end if
   end do
  
  end function get_nearest_nest_index
  

end module m_nest
!**************************************************  
       