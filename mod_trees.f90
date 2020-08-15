!**************************************************
module m_trees
!**************************************************
  use m_common
  use m_tools
  use m_geo
  
  implicit none

  private 
  integer, parameter :: X = 1
  integer, parameter :: Y = 2
  
  type t_tree
     integer :: treeID
     integer(kind = kCoord),dimension(2) :: coord
     character(50) :: tree_name
     logical :: swallow
     logical :: spitted
  end type t_tree

  type(t_tree), dimension(:), save, allocatable :: trees
  
 
  public :: t_tree
  public :: create_trees    
  public :: deallocate_trees   
  public :: print_trees   
  public :: get_tree
  public :: size_of_trees
     
  CONTAINS

!=================================================

character(80) FUNCTION FruitTreesFilePeriodFile()
!FruitTreesFilePeriod1: Feeding_area_Mar-July17.txt
!FruitTreesFilePeriod2: Feeding_area_Nov17-Mar18.txt
!FruitTreesFilePeriod3: Feeding_area_Aug-Oct18.txt 
      integer :: NrFichier   
           SELECT CASE (prm_i_month)
          CASE (4:7)
             NrFichier = 1  ! avril-juillet 2017 (04-05-06-07)
          CASE (8:10)
             NrFichier = 3  ! août-octobre  2018 (08-09-10)
          CASE DEFAULT
             NrFichier = 2  ! nov2017-mars 2018 (11-12-01-02-03)
       END SELECT
       FruitTreesFilePeriodFile = prm_c_FruitTreesFilePeriodFiles(NrFichier)

END FUNCTION FruitTreesFilePeriodFile
!=================================================   
   subroutine create_trees()
!=================================================   
    use m_tools
    implicit none
    
      character(80) :: filePath
      
      integer :: err, i,nbr_records, nbr_names, mm, true_false
       
      character(5),dimension(200) :: names
      character(160) :: str
      character(50),dimension(6) :: splitted_str
      character(10) :: tree_type
      character(80) :: textFile
  
	  
      nbr_records = 0
      nbr_names = 0
      
      textFile = FruitTreesFilePeriodFile()
         
       filePath = create_filepath(prm_c_input_dir, textFile)
          
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
 
      allocate(trees(nbr_records))

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
      !  write (*,*) str
! Aug 18	420	814585	1605105	Afzellia	0	1
! Mar 17	1	815014	1605465	NA	1	1
! May 17	57	814153	1603542	Willugbeia-Aglaia-Syzygium	1	1
! Dec 17	245	814720	1606071	Afzelia-Dialium coc.-Leucaena	1	1
        call split_string(trim(str(8:)),splitted_str,tab)
        READ(splitted_str(1), '(I)') trees(i)%treeID
        READ(splitted_str(2), '(I)') trees(i)%coord(X)
        READ(splitted_str(3), '(I)') trees(i)%coord(Y)
        trees(i)%tree_name = splitted_str(4)
        READ(splitted_str(5), '(I)') true_false
        trees(i)%swallow = (true_false > 0)
        READ(splitted_str(6), '(I)') true_false
        trees(i)%spitted = (true_false > 0)
      end do loop2
      close(1) 
      
      call print_trees(.FALSE.)

   end subroutine create_trees
 
 
!================================================= 
      subroutine deallocate_trees()
!=================================================      
      implicit none
      deallocate (trees)
     end subroutine deallocate_trees 
  
!================================================= 
      subroutine print_trees(OuputConsole)
!=================================================      
      implicit none
      LOGICAL, INTENT(IN) :: OuputConsole
      character(100) :: outfile
      integer :: i
      
      write (outfile, '(A,A)') trim(prm_c_output_dir), file_name('Feeding_area.csv')
      open(1,file=outfile,status='replace',action='write')
      write(1,'(11A)') 'Idx',tab,'X',tab,'Y',tab,'Name',tab,'Swallow',tab,'Spitted'
      write (1,'(I3,A,I7,A,I7,3A,L,A,L)') (trees(i)%treeID,tab,trees(i)%coord(X),tab,trees(i)%coord(Y), &     
                tab, trees(i)%tree_name(1:30),tab,trees(i)%swallow,tab,trees(i)%spitted, i=1,size(trees))
      close(1)
      if (OuputConsole) THEN
          write(*,*) '****************************************'
          write(*,'(A,1X,A,A,I3)') 'Fruit trees', trim(FruitTreesFilePeriodFile()), '- Mois: ' , prm_i_month
          write(*,*) '----------------------------------------------'
          write(*,'(6A,25X,3A)') 'Idx   X', tab,tab, 'Y',tab,'Name','Swallow',tab,'Spitted'
           write (*,'(I3,I7,2X,I7,3A,L,A,L)') (trees(i)%treeID,trees(i)%coord(X),trees(i)%coord(Y), &     
                tab, trees(i)%tree_name(1:30),tab,trees(i)%swallow,tab,trees(i)%spitted, i=1,size(trees))
      END IF 
     ! 

      
     end subroutine print_trees   
     

     ! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

   INTEGER FUNCTION  FindMinimum(x, Start, End)
      IMPLICIT  NONE
      double precision, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)                :: Start, End
      double precision                   :: Minimum
      INTEGER                            :: Location
      INTEGER                            :: i

      Minimum  = x(Start)		! assume the first is the min
      Location = Start			! record its position
      DO i = Start+1, End		! start with next elements
         IF (x(i) < Minimum) THEN	!   if x(i) less than the min?
            Minimum  = x(i)		!      Yes, a new minimum found
            Location = i                !      record its position
         END IF
      END DO
      FindMinimum = Location        	! return the position
   END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      type(t_tree), INTENT(INOUT) :: a, b
      type(t_tree)                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap
! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

   ! SUBROUTINE  Sort()
      ! IMPLICIT  NONE
      ! INTEGER                               :: i, taille
      ! INTEGER                               :: Location
      ! double precision, dimension(size(trees)) :: keys
      
      ! taille = size(trees)
      ! DO i = 1, taille-1			! except for the last
         ! keys = trees%P(X) + (trees%P(Y)*10E-7)
         ! Location = FindMinimum(keys, i, taille)	! find min from this to last
         ! CALL  Swap(trees(i), trees(Location))	! swap this and the minimum
      ! END DO
   ! END SUBROUTINE  Sort  

! --------------------------------------------------------------------
! BINARY SEARCH
!   the goal is to find the unique index i such that xi <= u < xi+1
! --------------------------------------------------------------------
integer function nearest_key( keys, u )
   real,dimension(:), intent(in) :: keys 
   real, intent(in) :: u 
   integer :: i,j,k
   i = 1
   j = size(keys)
   if (u >= keys(j)) then
     nearest_key = j
     return
   end if
   
   DO 
      k=(i+j)/2   
      IF (u < keys(k)) THEN 
        j=k  
      ELSE
        i=k
      END IF
      IF (i+1 >= j) EXIT
   END DO 
   nearest_key = i
 end function nearest_key
  
 
 
  type(t_tree) function get_tree(idx)
   integer, intent(in) :: idx
   get_tree = trees(idx)
  end function get_tree
  
  integer function size_of_trees()
      size_of_trees = size(trees)
  end function size_of_trees
!**************************************************
end module m_trees
!**************************************************


     