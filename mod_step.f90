!**************************************************
 module m_step
!**************************************************
  use m_common
  use cdf_gamma_mod 
  
  implicit none
  type t_step
    integer,private :: stateNr  ! numéro state
    double precision, private :: mean, sd, zero_mass
    ! https://en.wikipedia.org/wiki/Gamma_distribution
    !With a shape parameter k and a scale parameter ?.
    !With a shape parameter a = k and an inverse scale parameter ß = 1/?, 
    ! called a rate parameter.
    !With a shape parameter k and a mean parameter µ = k/ß.

    double precision, private :: shape, rate, scale
    double precision, private :: alpha, theta, beta
    
    
  contains
    procedure,pass(self),public :: init => init_step
    procedure,pass(self),public ::  toString => display_step
    procedure,pass(self),private :: GammaDist
    procedure,pass(self),private :: GammaDistCum
    procedure,pass(self),private :: InvGammaDistCum
    procedure,pass(self),private :: InvGammaDist
    procedure,pass(self),public :: TestSteps
    procedure,pass(self),public :: TestGamma
    procedure,pass(self),public :: Get_RandomStep
  end type t_step
  
  contains 

!===================================================================  
  subroutine init_step (self,nr, mean,sd, zero_mass)
!-------------------------------------------------------------------    
   class (t_step), intent(out) :: self
      integer, intent(in) :: nr
      double precision, intent(in) :: mean, sd, zero_mass
      
      self%stateNr = nr
      self%mean = mean
      self%sd = sd
      self%zero_mass = zero_mass
            
      self%shape = mean**2/sd**2    !! PDF file hMM page 19
      !self%rate = mean/sd**2
      !self%scale = 1.0d0 / self%rate 
      self%scale = mean/sd**2
      self%rate = 1.0d0 / self%scale 
            
      ! self%k = self%shape 
      self%alpha = self%shape 
      self%theta = self%scale
      self%beta = self%rate
               
    end subroutine init_step
    
!===================================================================  
  double precision function GammaDist(self, x)
!-------------------------------------------------------------------    
    class(t_step),intent(in) :: self 
    double precision, intent(in) :: x
    real( kind = 8 ) r8_gamma_pdf
    ! see http://www.real-statistics.com/other-key-distributions/gamma-distribution/
    ! see : http://keisan.casio.com/exec/system/1180573217


  !  write(*,*) 'alpha:', self%alpha, 'beta', self%beta
     GammaDist = ((x/self%beta)**(self%alpha - 1.0d0)) * exp(-1.0d0*x/self%beta) / &
           (self%beta * gamma(self%alpha))
    ! GammaDist = r8_gamma_pdf(self%rate,self%shape,x)      

  end function GammaDist
  
  !==========================================================    
  double precision function GammaDistCum(self, x)
!-------------------------------------------------------------------        
    class(t_step),intent(in) :: self 
    double precision, intent(in) :: x
    real ( kind = 8 ) :: r8_gamma_pdf
    
    integer :: sts
    logical :: check_input = .false.
      
    GammaDistCum = cum_gamma(x,self%shape,self%scale,sts,check_input)
   ! GammaDistCum = r8_gamma_pdf ( self%beta, self%alpha, x )
    if(sts /= 0) then
      write(*,*) "***************************************************"
      write(*,'(A,I3)') "Fatal error in cum_gamma function, status = ", sts
      write(*,'(3(A,F11.8))') "x=",x,", shape=",self%shape, ", scale=" , self%scale
      write(*,*) "***************************************************"
      stop 
    end if

   
  end function GammaDistCum
  
!==========================================================    
  double precision function InvGammaDistCum(self, cum)
!-------------------------------------------------------------------          
    class(t_step),intent(in) :: self 
    double precision, intent(in) :: cum
    double precision :: ccum
    
    integer :: sts
    logical :: check_input = .false.
    
    ccum = 1.0d0-cum
      
    InvGammaDistCum = inv_gamma(cum,ccum,self%shape,self%scale,sts,check_input)
    if(sts /= 0) then
      write(*,*) "***************************************************"
      write(*,'(A,I3)') "Fatal error in inv_gamma function, status = ", sts
      write(*,'(3(A,F11.8))') "cum=",cum,", shape=",self%shape, ", scale=" , self%scale
      write(*,*) "***************************************************"
      stop 
    end if

    
  end function InvGammaDistCum     

!=================================================================== 
  double precision function InvGammaDist(self, x)
!-------------------------------------------------------------------      
    class(t_step),intent(in) :: self
    double precision, intent(in) :: x
    
    ! see https://fr.wikipedia.org/wiki/Loi_inverse-gamma
    
    ! InvGammaDist = (self%beta ** self%alpha) / gamma(self%alpha) &
               ! * ((1.0d0 / x)**(self%alpha+1.0d0))  &
               ! * exp(self%beta*-1.0d0/x)
    InvGammaDist =  (self%beta ** self%alpha) / gamma(self%alpha) &
                * ((1.0d0 / x)**(-1.0d0*self%alpha-1.0d0))  &
                * exp(self%beta*-1.0d0/x)               

  end function InvGammaDist
  

!==================================================================
  double precision function get_RandomStep(self)
!-------------------------------------------------------------------      
    class(t_step),intent(in) :: self 
       
    double precision :: rnd
    call random_number(rnd)
    if( rnd <= self%zero_mass) then
       call random_number(rnd)  !! second trial
    end if
    get_RandomStep = self%InvGammaDistCum(rnd)
        
   end function get_RandomStep
   
!===================================================================
  subroutine TestSteps (self)
!-------------------------------------------------------------------        
      class(t_step),intent(in) :: self 

      integer, dimension(:), allocatable :: counts
      integer :: i, idx,total
      character(100) :: outfile
      character(15),dimension(2) :: str 
      integer, parameter :: nbrRandom = 10 ! 10E6
      integer, parameter :: nbrCounts = 150
      
      call random_seed() ! init random generator
      
      allocate(counts(0:nbrCounts+1))
      counts = 0
      
      do i= 1, nbrRandom 
        idx = nint(self%get_RandomStep())
        if(idx > nbrCounts) then
            idx = nbrCounts + 1
        end if    
        counts(idx) = counts(idx) + 1
      end do 
      total = sum(counts)
      write (outfile, '(A,A,I1,A)') trim(prm_c_output_dir), 'TestSteps_state', self%stateNr, '.txt'
      open(1,file=outfile,status='replace',action='write')
      write (1,'(53A)') "step_m",tab,"density"
      do i= 0, nbrCounts + 1
           write(str(1),*) i
           write(str(2),'(E11.4)') REAL(counts(i)) / REAL(total)
           write(1,'(3A)') trim(adjustl(str(1))), tab, trim(adjustl(str(2)))
      end do


      write(*,*) "TOTAL:", total
      deallocate (counts)
      
     
  end subroutine TestSteps
  
!==================================================================    
  subroutine TestGamma (self)
!
!   Test all the subroutines to compare to Excel results
!-------------------------------------------------------------------  
      class(t_step),intent(in) :: self 
      double precision,parameter :: xstart = 10.0d0, xend = 300.0d0, xstep = 10.0d0 
      double precision :: x, gammad, gammacum, invgamma
      
      write(*,*) '**************************************************'
      write(*,'(A,I8)') 'State : ', self%stateNr 
      write(*,*) ''
      write(*,*) 'Shape : ', self%shape
      write(*,*) 'Rate  : ', self%rate
      write(*,*) ''
      write(*,*) '  X         Gamma dist      Gamma cum   inv cum' 
      write(*,*) '  -         ----------      ---------   -------'
      do x = xstart, xend, xstep
            gammad = self%GammaDist(x)
            gammacum = self%GammaDistCum(x)
            invgamma = self%InvGammaDistCum(gammacum)
            write(*,'(F7.2,2(E16.8),F9.2)') x, gammad, gammacum, invgamma
      end do
      write(*,*) '****************************************************'
    
  end subroutine TestGamma
  

!===================================================================    
    subroutine display_step(self)
!-------------------------------------------------------------------      
     class (t_step), intent(in) :: self
      write (*,'(A,E17.9)') 'mean     : ', self%mean
      write (*,'(A,E17.9)') 'sd       : ', self%sd
      write (*,'(A,E17.9)') 'zero-mass: ', self%zero_mass  
      write (*,'(2(A,E17.9))') 'step shape ', self%shape, '  beta ',  self%beta
   
    end subroutine display_step
    

!**************************************************
 end module m_step
!**************************************************