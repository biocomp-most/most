!**************************************************
 module m_angle
!**************************************************

  use m_common
 
  implicit none
  
  type t_angle
    integer,private :: stateNr  ! numéro state
    double precision, private :: mean, concentration, mu
    double precision, private :: k,tau,rho,r
    
  contains
    procedure,pass(self),public  :: init => init_angle
    procedure,pass(self),public  :: toString => display_angle
    procedure,pass(self),public :: Get_RandomAngle
    procedure,pass(self),public :: testAngles

  end type t_angle

CONTAINS

!==============================================================   
 subroutine init_angle (self, nr, mean,concentration)
!--------------------------------------------------------------
    class (t_angle), intent(out) :: self
    integer, intent(in) :: nr
    double precision, intent(in) :: mean,concentration

    self%stateNr = nr
    self%mean = mean ! 0.0d0
    self%concentration = concentration
    self%k = concentration
    self%mu = self%mean


    self%tau = 1.0D+00 + sqrt ( 1.0D+00 + 4.0D+00 * concentration * concentration )
    self%rho = ( self%tau - sqrt ( 2.0D+00 * self%tau ) ) / ( 2.0D+00 * concentration )
    self%r = ( 1.0D+00 + self%rho ** 2 ) / ( 2.0D+00 * self%rho )

 end subroutine init_angle 

!==============================================================   
 subroutine display_angle(self)
!-------------------------------------------------------------- 
    class (t_angle), intent(in) :: self
      write (*,'(2(A,F11.8))') 'Angle : mean', self%mean, '  conc.',  self%concentration
 
 end subroutine display_angle
    
 
!==============================================================   
 double precision function get_RandomAngle(self)
!
! subroutine von_mises_sample ( a, b, seed, x )
! voir : http://people.sc.fsu.edu/~jburkardt/f_src/f_src.html
!  http://people.sc.fsu.edu/~jburkardt/f_src/prob/prob.html (prob.f90)
!-----------------------------------------------------------------------
 
  implicit none
  class(t_angle) :: self 
        
!=========================================================
!
!! VON_MISES_SAMPLE samples the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!  Parameters:
!     A = mean
!     B = concentration 
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  double precision c,f,U1,U2,U3,z

  do
    call random_number(U1)
    z = cos ( M_PI * u1 )
    f = ( 1.0D+00 + self%r * z ) / ( self%r + z )
    c = self%concentration * ( self%r - f )

    call random_number(U2)

    if ( u2 < c * ( 2.0D+00 - c ) ) then
      exit
    end if

    if ( c <= log ( c / u2 ) + 1.0D+00 ) then
      exit
    end if

  end do

  call random_number(U3)
  get_RandomAngle = self%mean + sign ( 1.0D+00, u3 - 0.5D+00 ) * acos ( f )

end function get_RandomAngle
  

 
!==========================================================    
    subroutine TestAngles (self)
!--------------------------------------------------------------    
    class(t_angle),intent(in) :: self 

    integer, dimension(:), allocatable :: counts
    integer :: nbrRandom, i, idx,total
    integer :: minIdx
    integer :: maxIdx
    character(100) :: outfile
    character(15),dimension(2) :: str 
    double precision :: rnd_angle, toto, rnd2

    minIdx = -280
    maxIdx = +280
    call random_seed() ! init random generator
    nbrRandom = 1E07


    allocate(counts(minIdx:maxIdx))
    counts = 0

    do i= 1, nbrRandom 
    toto = self%get_RandomAngle()
   ! call random_number(rnd2)
    ! call von_mises_cdf_inv ( rnd2, self%mean, self%concentration, toto )
    rnd_angle = 180.0d0 * toto / M_PI
    idx = nint(rnd_angle)
    !        if(idx == -180) then; idx = 180; end if 
    if(idx < minIdx .or. idx > maxidx) then 
      write (*,*) toto, rnd_angle, idx
      stop
    end if
    counts(idx) = counts(idx) + 1

    end do 
    do i=minIdx,0
       if (counts(i) > 0.0d0) then
          minIdx = i
          exit
       end if
    end do
    do i=maxIdx,0,-1
       if (counts(i) > 0.0d0) then
          maxIdx = i
          exit
       end if
    end do

    counts(maxIdx) = counts(maxIdx) + counts(minIdx)
    minIdx = minIdx + 1
    total = sum(counts(minIdx:maxIdx))
    ! counts(180) = counts(180) + counts(-180)
    write (outfile, '(A,A,I1,A)') trim(prm_c_output_dir), 'TestAngles_state', self%stateNr, '.txt'
    open(1,file=outfile,status='replace',action='write')
    write (1,'(53A)') "degrees",tab,"density"
    do i= minIdx,maxIdx
       write(str(1),*) i
       write(str(2),'(F11.4)') REAL(counts(i)) / REAL(total)
       write(1,'(3A)') trim(adjustl(str(1))), tab, trim(adjustl(str(2)))
    end do
    close(1)

    write(*,*) "TOTAL:", total
    deallocate (counts)


    end subroutine TestAngles

    


!**************************************************
 end module m_angle
!**************************************************